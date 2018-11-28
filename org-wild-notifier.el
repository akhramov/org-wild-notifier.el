;;; org-wild-notifier.el --- Customizable org-agenda notifications -*- lexical-binding: t -*-

;; Copyright (C) 2017 Artem Khramov

;; Author: Artem Khramov <futu.fata@gmail.com>
;; Created: 6 Jan 2017
;; Version: 0.2.4
;; Package-Requires: ((alert "1.2") (dash "2.13.0") (emacs "24.4"))
;; Keywords: notification alert org org-agenda agenda
;; URL: https://github.com/akhramov/org-wild-notifier.el

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
;;
;; To perform a one-time check use `org-wild-notifier-check'
;; function.
;; To enable timer-based notifications please use
;;`org-wild-notifier-mode'.
;; Notification times can be customized either globally (for all org
;; entries) through `org-wild-notifier-alert-time' variable or on per
;; org entry basis using `WILD_NOTIFIER_NOTIFY_BEFORE` property, which
;; in turn is customizable via
;; `org-wild-notifier-alert-times-property' variable.
;; By default you get notifications about TODO events only.  To
;; customize that behavior please use
;; `org-wild-notifier-keyword-whitelist' variable.  In contrary, if
;; you don't want to receive notifications regarding certain events,
;; you can use `org-wild-notifier-keyword-blacklist' variable.

;;; Code:

(require 'dash)
(require 'alert)
(require 'org-agenda)


(defgroup org-wild-notifier nil
  "org-wild-notifier customization options"
  :group 'org)

(defcustom org-wild-notifier-alert-time 10
  "Time in minutes to get a notification about upcomming event.
Cannot be less than 1."
  :package-version '(org-wild-notifier . "0.1.0")
  :group 'org-wild-notifier
  :type 'integer)

(defcustom org-wild-notifier-alert-times-property "WILD_NOTIFIER_NOTIFY_BEFORE"
  "Use this property in your agenda files to add additional notifications \
to an event."
  :package-version '(org-wild-notifier . "0.1.0")
  :group 'org-wild-notifier
  :type 'string)

(defcustom org-wild-notifier-notification-title "Agenda"
  "Notifications title."
  :package-version '(org-wild-notifier . "0.1.0")
  :group 'org-wild-notifier
  :type 'string)

(defcustom org-wild-notifier-keyword-whitelist '("TODO")
  "Receive notifications for these keywords only.
Leave this variable blank if you do not want to filter anything."
  :package-version '(org-wild-notifier . "0.2.2")
  :group 'org-wild-notifier
  :type '(repeat string))

(defcustom org-wild-notifier-keyword-blacklist nil
  "Never receive notifications for these keywords."
  :package-version '(org-wild-notifier . "0.2.2")
  :group 'org-wild-notifier
  :type '(repeat string))

(defvar org-wild-notifier--day-wide-events nil
  "If truthy, notifies about day-wide events.")

(defvar org-wild-notifier--timer nil
  "Timer value.")

(defvar org-wild-notifier--agenda-buffer-name "*org wild notifier affairs*"
  "A name for temporary 'org-agenda' buffer.")

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
      ;; SPIKE: Org timestamps without "time" section are shorter than
      ;; 16 characters.
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
  (-> seconds
       (pcase
         ((pred (>= 0)) "today")
         ((pred (>= 3600)) "in %M")
         (_ "in %H %M"))

       (format-seconds seconds)))

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

(defun org-wild-notifier--entry-whitelisted-p (marker)
  "Check if MARKER is whitelisted."
  (-contains-p org-wild-notifier-keyword-whitelist
               (org-entry-get marker "TODO")))

(defun org-wild-notifier--apply-whitelist (markers)
  "Apply whitelist to MARKERS."
  (if org-wild-notifier-keyword-whitelist
      (-filter 'org-wild-notifier--entry-whitelisted-p markers)
    markers))

(defun org-wild-notifier--entry-blacklisted-p (marker)
  "Check if MARKER is blacklisted."
  (-contains-p org-wild-notifier-keyword-blacklist
               (org-entry-get marker "TODO")))

(defun org-wild-notifier--apply-blacklist (markers)
  "Apply blacklist to MARKERS."
  (if org-wild-notifier-keyword-blacklist
      (-remove 'org-wild-notifier--entry-blacklisted-p markers)
    markers))

(defun org-wild-notifier--retrieve-events ()
  "Get events from agenda view."
  (->> (org-split-string (buffer-string) "\n")
       (--map (plist-get
               (org-fix-agenda-info (text-properties-at 0 it))
               'org-marker))
       (-non-nil)
       (org-wild-notifier--apply-whitelist)
       (org-wild-notifier--apply-blacklist)
       (-map 'org-wild-notifier--gather-info)))

(defun org-wild-notifier--notify (event-msg)
  "Notify about an event using `alert' library.
EVENT-MSG is a string representation of the event."
  (alert event-msg :title org-wild-notifier-notification-title))

(defun org-wild-notifier--extract-time (marker)
  "Extract timestamps from MARKER.
Timestamps are extracted as cons cells.  car holds org-formatted
string, cdr holds time in list-of-integer format."
  (-non-nil
   (--map
    (let ((org-timestamp (org-entry-get marker it)))
      (and org-timestamp
           (cons org-timestamp
                 (apply 'encode-time (org-parse-time-string org-timestamp)))))
    '("DEADLINE" "SCHEDULED" "TIMESTAMP"))))

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


(defun org-wild-notifier--stop ()
  "Stops the notification timer."
  (-some-> org-wild-notifier--timer cancel-timer))

(defun org-wild-notifier--start ()
  "Start the notification timer.  Cancel old one, if any.
Timer is scheduled on the beginning of every minute, so for
smoother experience this function also runs a check without timer."
  (org-wild-notifier--stop)

  (let ((org-wild-notifier--day-wide-events t))
    (org-wild-notifier-check))

  (--> (format-time-string "%H:%M" (time-add (current-time) 60))
       (run-at-time it 60 'org-wild-notifier-check)
       (setf org-wild-notifier--timer it)))

;;;###autoload
(defun org-wild-notifier-check ()
  "Parse agenda view and notify about upcomming events."
  (interactive)
  (save-window-excursion
    (let ((org-agenda-use-time-grid nil)
          (org-agenda-compact-blocks t)
          (org-agenda-window-setup 'current-window)
          (org-agenda-buffer-name nil)
          (org-agenda-buffer-tmp-name org-wild-notifier--agenda-buffer-name))

      (org-agenda-list 2)

      (-each
        (->> (org-wild-notifier--retrieve-events)
             (-map 'org-wild-notifier--check-event)
             (-flatten)
             (-uniq))
        'org-wild-notifier--notify)

      (org-agenda-exit))))

;;;###autoload
(define-minor-mode org-wild-notifier-mode
  "Toggle org notifications globally.
When enabled parses your agenda once a minute and emits notifications
if needed."
  :global
  :lighter "Org Wild Notifier"
  (if org-wild-notifier-mode
      (org-wild-notifier--start)
    (org-wild-notifier--stop)))

(provide 'org-wild-notifier)

;;; org-wild-notifier.el ends here
