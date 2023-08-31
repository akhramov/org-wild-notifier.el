;;; org-wild-notifier.el --- Customizable org-agenda notifications -*- lexical-binding: t -*-


;; Copyright (C) 2017 Artem Khramov

;; Author: Artem Khramov <akhramov+emacs@pm.me>
;; Created: 6 Jan 2017
;; Version: 0.4.1
;; Package-Requires: ((alert "1.2") (async "1.9.3") (dash "2.18.0") (emacs "24.4"))
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
(require 'async)
(require 'org-agenda)
(require 'cl-lib)


(defgroup org-wild-notifier nil
  "org-wild-notifier customization options"
  :group 'org)

(defcustom org-wild-notifier-alert-time '(10)
  "Time in minutes to get a notification about upcoming event.
Cannot be less than 1."
  :package-version '(org-wild-notifier . "0.1.0")
  :group 'org-wild-notifier
  :type '(choice (integer :tag "Notify once")
                 (repeat integer)))

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

(defcustom org-wild-notifier-notification-icon nil
  "Path to notification icon file."
  :package-version '(org-wild-notifier . "0.4.1")
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

(defcustom org-wild-notifier-tags-whitelist nil
  "Receive notifications for these tags only.
Leave this variable blank if you do not want to filter anything."
  :package-version '(org-wild-notifier . "0.3.1")
  :group 'org-wild-notifier
  :type '(repeat string))

(defcustom org-wild-notifier-tags-blacklist nil
  "Never receive notifications for these tags."
  :package-version '(org-wild-notifier . "0.3.1")
  :group 'org-wild-notifier
  :type '(repeat string))

(defcustom org-wild-notifier--alert-severity 'medium
  "Severity of the alert.
options: 'high 'medium 'low"
  :package-version '(org-wild-notifier . "0.3.1")
  :group 'org-wild-notifier
  :type 'symbol
  :options '(high medium low))

(defcustom org-wild-notifier-day-wide-alert-times nil
  "A list of time of day strings at which alerts for day wide events should trigger."
  :package-version '(org-wild-notifier . "0.3.1")
  :group 'org-wild-notifier
  :type 'string)

(defvar org-wild-notifier--day-wide-events nil
  "If truthy, notifies about day-wide events.")

(defvar org-wild-notifier--timer nil
  "Timer value.")

(defvar org-wild-notifier--process nil
  "Currently-running async process.")

(defvar org-wild-notifier--agenda-buffer-name "*org wild notifier affairs*"
  "A name for temporary 'org-agenda' buffer.")

(defvar org-wild-notifier--last-check-time (seconds-to-time 0)
  "Last time checked for events.")

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

(defun org-wild-notifier--timestamp-within-interval-p (timestamp interval)
  "Check whether TIMESTAMP is within notification INTERVAL."
  (org-wild-notifier--time=
   (time-add (current-time) (seconds-to-time (* 60 interval)))
   timestamp))

(defun org-wild-notifier--notifications (event)
  "Get notifications for given EVENT.
Returns a list of time information interval pairs."
  (->> (list
        (org-wild-notifier--filter-day-wide-events (cadr (assoc 'times event)))
        (cdr (assoc 'intervals event)))
         (apply '-table-flat (lambda (ts int) (list ts int)))
         ;; When no values are provided for table flat, we get the second values
         ;; paired with nil.
         (--filter (not (null (car it))))
         (--filter (org-wild-notifier--timestamp-within-interval-p (cdar it) (cadr it)))))

(defun org-wild-notifier--has-timestamp (s)
  (string-match org-ts-regexp0 s)
  (match-beginning 7))

(defun org-wild-notifier--filter-day-wide-events (times)
  (if org-wild-notifier--day-wide-events
      times
    (--filter (org-wild-notifier--has-timestamp (car it)) times)))

(defun org-wild-notifier--time-left (seconds)
  "Human-friendly representation for SECONDS."
  (-> seconds
       (pcase
         ((pred (>= 0)) "right now")
         ((pred (>= 3600)) "in %M")
         (_ "in %H %M"))

       (format-seconds seconds)))

(defun org-wild-notifier--get-hh-mm-from-org-time-string (time-string)
  "Convert given org time-string TIME-STRING into string with 'hh:mm' format."
  (if (string-match "\\([0-9]+\\):\\([0-9]+\\)" time-string)
      (format "%s:%s"
              (string-to-number (match-string 1 time-string))
              (string-to-number (match-string 2 time-string)))
    "00:00"))

(defun org-wild-notifier--notification-text (str-interval event)
  "For given STR-INTERVAL list and EVENT get notification wording."
  (format "%s at %s (%s)"
          (cdr (assoc 'title event))
          (org-wild-notifier--get-hh-mm-from-org-time-string (car str-interval))
          (org-wild-notifier--time-left (* 60 (cdr str-interval)))))

(defun org-wild-notifier-get-minutes-into-day (time)
  (org-duration-to-minutes (org-get-time-of-day time t)))

(defun org-wild-notifier-get-hours-minutes-from-time (time-string)
  (let ((total-minutes (truncate (org-wild-notifier-get-minutes-into-day time-string))))
    (list (/ total-minutes 60)
          (mod total-minutes 60))))

(defun org-wild-notifier-set-hours-minutes-for-time (time hours minutes)
  (cl-destructuring-bind (_s _m _h day month year dow dst utcoff) (decode-time time)
    (encode-time 0 minutes hours day month year dow dst utcoff)))

(defun org-wild-notifier-current-time-matches-time-of-day-string (time-of-day-string)
  (let ((now (current-time)))
    (org-wild-notifier--time=
     now
     (apply 'org-wild-notifier-set-hours-minutes-for-time
            now
            (org-wild-notifier-get-hours-minutes-from-time time-of-day-string)))))

(defun org-wild-notifier-current-time-is-day-wide-time ()
  (--any (org-wild-notifier-current-time-matches-time-of-day-string it)
         org-wild-notifier-day-wide-alert-times))

(defun org-wild-notifier-day-wide-notifications (events)
  (->> events
       (-filter 'org-wild-notifier-event-has-any-day-wide-timestamp)
       (-map 'org-wild-notifier--day-wide-notification-text)
       (-uniq)))

(defun org-wild-notifier-event-has-any-day-wide-timestamp (event)
  (--any (not (org-wild-notifier--has-timestamp (car it)))
         (car (cdr (assoc 'times event )))))

(defun org-wild-notifier--day-wide-notification-text (event)
  "For given STR-INTERVAL list and EVENT get notification wording."
  (format "%s is due or scheduled today"
          (cdr (assoc 'title event))))

(defun org-wild-notifier--check-event (event)
  "Get notifications for given EVENT.
Returns a list of notification messages"
  (->> (org-wild-notifier--notifications event)
       (--map (org-wild-notifier--notification-text `(,(caar it) . ,(cadr it)) event))))

(defun org-wild-notifier--get-tags (marker)
  "Retrieve tags of MARKER."
  (-> (org-entry-get marker "TAGS")
      (or "")
      (org-split-string  ":")))

(defun org-wild-notifier--whitelist-predicates ()
  (->> `([,org-wild-notifier-keyword-whitelist
          (lambda (it)
            (-contains-p org-wild-notifier-keyword-whitelist
                         (org-entry-get it "TODO")))]

         [,org-wild-notifier-tags-whitelist
          (lambda (it)
            (-intersection org-wild-notifier-tags-whitelist
                           (org-wild-notifier--get-tags it)))])
       (--filter (aref it 0))
       (--map (aref it 1))))

(defun org-wild-notifier--blacklist-predicates ()
  (->> `([,org-wild-notifier-keyword-blacklist
          (lambda (it)
            (-contains-p org-wild-notifier-keyword-blacklist
                         (org-entry-get it "TODO")))]

         [,org-wild-notifier-tags-blacklist
          (lambda (it)
            (-intersection org-wild-notifier-tags-blacklist
                           (org-wild-notifier--get-tags it)))])
       (--filter (aref it 0))
       (--map (aref it 1))))

(defun org-wild-notifier--apply-whitelist (markers)
  "Apply whitelist to MARKERS."
  (-if-let (whitelist-predicates (org-wild-notifier--whitelist-predicates))
      (-> (apply '-orfn whitelist-predicates)
          (-filter markers))
    markers))

(defun org-wild-notifier--apply-blacklist (markers)
  "Apply blacklist to MARKERS."
  (-if-let (blacklist-predicates (org-wild-notifier--blacklist-predicates))
      (-> (apply '-orfn blacklist-predicates)
          (-remove markers))
    markers))

(defun org-wild-notifier--retrieve-events ()
  "Get events from agenda view."
  (let ((agenda-files (-filter 'file-exists-p (org-agenda-files)))
        ;; Some package managers manipulate `load-path` variable.
        (my-load-path load-path)
        (todo-keywords org-todo-keywords)
        (alert-time org-wild-notifier-alert-time)
        (keyword-whitelist org-wild-notifier-keyword-whitelist)
        (keyword-blacklist org-wild-notifier-keyword-blacklist)
        (tags-whitelist org-wild-notifier-tags-whitelist)
        (tags-blacklist org-wild-notifier-tags-blacklist))
    (lambda ()
      (setf org-agenda-use-time-grid nil)
      (setf org-agenda-compact-blocks t)
      (setf org-agenda-files agenda-files)
      (setf load-path my-load-path)
      (setf org-todo-keywords todo-keywords)
      (setf org-wild-notifier-alert-time alert-time)
      (setf org-wild-notifier-keyword-whitelist keyword-whitelist)
      (setf org-wild-notifier-keyword-blacklist keyword-blacklist)
      (setf org-wild-notifier-tags-whitelist tags-whitelist)
      (setf org-wild-notifier-tags-blacklist tags-blacklist)

      (package-initialize)
      (require 'org-wild-notifier)

      (org-agenda-list 2
                       (org-read-date nil nil "today"))

      (->> (org-split-string (buffer-string) "\n")
           (--map (plist-get
                   (org-fix-agenda-info (text-properties-at 0 it))
                   'org-marker))
           (-non-nil)
           (org-wild-notifier--apply-whitelist)
           (org-wild-notifier--apply-blacklist)
           (-map 'org-wild-notifier--gather-info)))))

(defun org-wild-notifier--notify (event-msg)
  "Notify about an event using `alert' library.
EVENT-MSG is a string representation of the event."
  (alert event-msg
         :icon org-wild-notifier-notification-icon
         :title org-wild-notifier-notification-title
         :severity org-wild-notifier--alert-severity))

(defun org-wild-notifier--timestamp-parse (timestamp)
  (let ((parsed (org-parse-time-string timestamp))
        (today (org-format-time-string "<%Y-%m-%d>")))
    ;; seconds-to-time returns also milliseconds and nanoseconds so we
    ;; have to "trim" the list
    (butlast 
     (seconds-to-time
      (org-time-add
       ;; we get the cycled absolute day (not hour and minutes)
       (org-time-from-absolute (org-closest-date timestamp today 'past))
       ;; so we have to add the minutes too
       (+ (* (decoded-time-hour parsed) 3600)
          (* (decoded-time-minute parsed) 60))))
     2)
))

(defun org-wild-notifier--extract-time (marker)
  "Extract timestamps from MARKER.
Timestamps are extracted as cons cells.  car holds org-formatted
string, cdr holds time in list-of-integer format."
  (-non-nil
   (--map
    (let ((org-timestamp (org-entry-get marker it)))
      (and org-timestamp
           (cons org-timestamp
                 (org-wild-notifier--timestamp-parse org-timestamp))))
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
  `(,@(-flatten (list org-wild-notifier-alert-time))
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
  "Stops the notification timer and cancel any in-progress checks."
  (-some-> org-wild-notifier--timer (cancel-timer))
  (when org-wild-notifier--process
    (interrupt-process org-wild-notifier--process)
    (setq org-wild-notifier--process nil)))

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

(defun org-wild-notifier--check-events (events)
  (setq org-wild-notifier--process nil)
  (-each
      (->> events
           (-map 'org-wild-notifier--check-event)
           (-flatten)
           (-uniq))
    'org-wild-notifier--notify)
  (when (org-wild-notifier-current-time-is-day-wide-time)
    (-map 'org-wild-notifier--notify
          (org-wild-notifier-day-wide-notifications events)))
  (setq org-wild-notifier--last-check-time (current-time)))

;;;###autoload
(defun org-wild-notifier-check ()
  "Parse agenda view and notify about upcoming events.

Do nothing if a check is already in progress in the background."
  (interactive)
  (unless (and org-wild-notifier--process
               (process-live-p org-wild-notifier--process))
    (setq org-wild-notifier--process
          (let ((default-directory user-emacs-directory))
            (async-start
             (org-wild-notifier--retrieve-events)
             'org-wild-notifier--check-events)))))

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
