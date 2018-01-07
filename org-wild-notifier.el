;;; org-wild-notifier.el --- Customizable org-agenda notifications

;; Copyright (C) 2017 Artem Khramov

;; Author: Artem Khramov <futu.fata@gmail.com>
;; Created: 6 Jan 2017
;; Version: 0.1
;; Package-Requires: ((alert "1.2") (dash "2.13.0"))
;; Keywords: notification alert org org-agenda agenda
;; URL: https://github.com/akhramov/org-wild-notifier

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides notification functions for org-agenda.
;; Notification times can be customized either globally (for all org
;; entries) through `org-wild-notifier-alert-time' variable or on per
;; org entry basis using `WILD_NOTIFIER_NOTIFY_BEFORE` property, which
;; in turn is customizable via
;; `org-wild-notifier-alert-times-property' variable.

;;; Code:

(require 'dash)
(require 'alert)
(require 'org-agenda)

(defvar org-wild-notifier-alert-time 10
  "Time in minutes to get a notification about upcomming event.
Cannot be less than 1.")

(defvar org-wild-notifier-alert-times-property "WILD_NOTIFIER_NOTIFY_BEFORE"
  "Use this property in your agenda files to add additional notifications \
to an event.")

(defvar org-wild-notifier-notification-title "Agenda"
  "Notifications title.")

(defvar org-wild-notifier--day-wide-events nil
  "If truthy, notifies about day-wide events.")

(defvar org-wild-notifier--timer nil
  "Timer value.")

(defun org-wild-notifier--time= (&rest list)
  "Compare timestamps.
Comparison is performed by converted each element of LIST onto string
in order to ignore seconds."
  (->> list
       (--map (format-time-string "%d:%H:%M" it))
       (-uniq)
       (length)
       (= 1)))

(defun org-wild-notifier--today ()
  "Get the timestamp for the beginning of current day."
  (apply 'encode-time
         (append '(0 0 0) (nthcdr 3 (decode-time (current-time))))))

(defun org-wild-notifier--always-notify-p (event)
  "Check that notification for the EVENT should be done regardless of time.
For now, the only case that handled is day-wide events."
  (when org-wild-notifier--day-wide-events
    (let ((today (org-wild-notifier--today)))
      (--any-p (and (<= (length (car it)) 16) (equal today (cdr it)))
               (cadr (assoc 'times event))))))

(defun org-wild-notifier--timestamp-within-interval-p (timestamp interval)
  "Check whether TIMESTAMP is within notification INTERVAL."
  (org-wild-notifier--time=
   (time-add (current-time) (seconds-to-time (* 60 interval)))
   timestamp))

(defun org-wild-notifier--notifications (event)
  "Get notifications for given EVENT.
Returns a list of notification intervals."
  (if (org-wild-notifier--always-notify-p event)
      '(-1)

    (->> `(,(cadr (assoc 'times event)) ,(cdr (assoc 'intervals event)))
         (apply '-table-flat (lambda (ts int) `(,(cdr ts) ,int)))
         (--filter (apply 'org-wild-notifier--timestamp-within-interval-p it))
         (-map 'cadr))))

(defun org-wild-notifier--time-left (seconds)
  "Human-friendly representation for SECONDS."
  (--> seconds
       (pcase it
         ((pred (>= 0)) "today")
         ((pred (>= 3600)) "in %M")
         (_ "in %H %M"))

       (format-seconds it seconds)))

(defun org-wild-notifier--notification-text (interval event)
  "For given INTERVAL and EVENT get notification wording."
  (format "%s %s"
          (cdr (assoc 'title event))
          (org-wild-notifier--time-left (* 60 interval))))

(defun org-wild-notifier--check-event (event)
  "Get notifications for given EVENT.
Returns a list of notification messages"
  (->> (org-wild-notifier--notifications event)
       (--map (org-wild-notifier--notification-text it event))))

(defun org-wild-notifier--retrieve-events ()
  "Get events from agenda view."
  (->> (org-split-string (buffer-string) "\n")
       (--map (plist-get
               (org-fix-agenda-info (text-properties-at 0 it))
               'org-marker))
       (--filter (equal "TODO" (org-entry-get it "TODO")))
       (-map 'org-wild-notifier--gather-info)))

(defun org-wild-notifier--notify (event-msg)
  "Notify about an event using `alert' library.
EVENT-MSG is a string representation of the event."
  (alert event-msg :title org-wild-notifier-notification-title))

(defun org-wild-notifier--extract-time (marker)
  "Extract timestamps from MARKER.
Timestamps are extracted as cons cells.  car holds org-formatted
string, cdr holds time in list-of-integer format."
  (remove nil
   (--map
    (let ((org-timestamp (org-entry-get marker it)))
      (and org-timestamp
           (cons org-timestamp
                 (apply 'encode-time (org-parse-time-string org-timestamp)))))
    '("DEADLINE" "SCHEDULED"))))

(defun org-wild-notifier--extract-title (marker)
  "Extract event title from MARKER.
MARKER acts like the event's identifier."
  (org-with-point-at marker
    (-let (((_lvl _reduced-lvl _todo _priority title _tags)
            (org-heading-components)))
      title)))

(defun org-wild-notifier--extract-notication-intervals (marker)
  "Extract notification intervals from the event's properties.
MARKER acts like the event's identifier.  Resulting list also contains
standard notification interval (`org-wild-notifier-alert-time')."
  `(,org-wild-notifier-alert-time
    ,@(-map 'string-to-number
           (org-entry-get-multivalued-property
            marker
            org-wild-notifier-alert-times-property))))

(defun org-wild-notifier--gather-info (marker)
  "Collect information about an event.
MARKER acts like event's identifier."
  `((times . (,(org-wild-notifier--extract-time marker)))
    (title . ,(org-wild-notifier--extract-title marker))
    (intervals . ,(org-wild-notifier--extract-notication-intervals marker))))

;;;###autoload
(defun org-wild-notifier-check ()
  "Parse agenda view and notify about upcomming events."
  (interactive)
  (save-window-excursion
    (let ((org-agenda-use-time-grid nil)
          (org-agenda-compact-blocks t)
          (org-agenda-window-setup 'current-window))

      (org-agenda-list 2)

      (-each
        (->> (org-wild-notifier--retrieve-events)
             (-map 'org-wild-notifier--check-event)
             (-flatten)
             (-uniq))
        'org-wild-notifier--notify)

      (kill-buffer))))

;;;###autoload
(defun org-wild-notifier-stop ()
  "Stops the notification timer."
  (interactive)
  (-some-> org-wild-notifier--timer cancel-timer))

;;;###autoload
(defun org-wild-notifier-start ()
  "Start the notification timer.  Cancel old one, if any.
Timer is scheduled on the beginning of every minute, so for
smoother experience this function also runs a check without timer."
  (interactive)
  (org-wild-notifier-stop)

  (let ((org-wild-notifier--day-wide-events t))
    (org-wild-notifier-check))

  (--> (format-time-string "%H:%M" (time-add (current-time) 60))
       (run-at-time it 60 'org-wild-notifier-check)
       (setf org-wild-notifier--timer it)))

(provide 'org-wild-notifier)

;;; org-wild-notifier.el ends here
