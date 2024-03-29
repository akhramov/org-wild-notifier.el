;;; org-wild-notifier-tests.el --- Tests for org-wild-notifier

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
;; entries) through `org-wild-notifier-alert-time` variable or on per
;; org entry basis using `WILD_NOTIFIER_NOTIFY_BEFORE` property, which
;; in turn is customizable via
;; `org-wild-notifier-alert-times-property` variable.

;;; Code:

(require 'ert)
(require 'org-wild-notifier)
(require 'dash)
(require 'subr-x)

(defun async-start (fn callback)
  (->> (funcall fn)
       (funcall callback)))

(cl-defmacro org-wild-notifier-test
    (test-name desc
               &key
               (time "15:30")
               (fixture "planning.org")
               (overrides nil)
               expected-alerts)
  `(ert-deftest ,test-name ()
     ,desc
     (cl-letf* ((org-agenda-files (list (expand-file-name ,fixture "fixtures")))
                (org-todo-keywords (list "TODO" "IN PROGRESS" "DONE"))
                (time-string (format "January 4, 2018 %s" ,time))
                ((symbol-function 'current-time)
                 (lambda () (apply 'encode-time (parse-time-string time-string))))
                (org-agenda-start-day (time-to-days (current-time)))
                ((symbol-function 'org-today)
                 (lambda () (time-to-days (current-time))))
                (registered-alerts nil)
                ((symbol-function 'alert)
                 (lambda (msg &rest _)
                   (setf registered-alerts (push msg registered-alerts))))
                ,@overrides)
       (progn
         (org-wild-notifier-check)
         (should (equal '(,@expected-alerts) registered-alerts))))))

(org-wild-notifier-test alert-time
  "Tests that it notifies before standard notification time (10 minutes)"
  :time "15:50"
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 10 minutes)"
   "TODO event at 16:00 with NOTIFY_BEFORE property set to 31 at 16:00 (in 10 minutes)"
   "TODO event at 16:00 with notifications before 80, 60, 55, 43 and 5 at 16:00 (in \
10 minutes)"
   "TODO event scheduled on 16:00 with deadline at 17:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test alert-time-overriden
  "Test that standard alert time can be customized"
  :time "14:50"
  :overrides ((org-wild-notifier-alert-time 70))
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 1 hour 10 minutes)"
   "TODO event at 16:00 with NOTIFY_BEFORE property set to 31 at 16:00 (in \
1 hour 10 minutes)"
   "TODO event at 16:00 with notifications before 80, 60, 55, 43 and 5 at 16:00 (in \
1 hour 10 minutes)"
   "TODO event scheduled on 16:00 with deadline at 17:00 at 16:00 (in \
1 hour 10 minutes)"))

(org-wild-notifier-test alert-time-overriden-with-list
  "Test that standard alert time can be customized & set to list"
  :time "14:50"
  :overrides ((org-wild-notifier-alert-time '(10 70)))
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 1 hour 10 minutes)"
   "TODO event at 16:00 with NOTIFY_BEFORE property set to 31 at 16:00 (in \
1 hour 10 minutes)"
   "TODO event at 16:00 with notifications before 80, 60, 55, 43 and 5 at 16:00 (in \
1 hour 10 minutes)"
   "TODO event scheduled on 16:00 with deadline at 17:00 at 16:00 (in \
1 hour 10 minutes)"
   "TODO event at 15:00 at 15:00 (in 10 minutes)"))

(org-wild-notifier-test notification-property
  "Tests that notifier takes in account the notification property"
  :time "15:17"
  :expected-alerts
  ("TODO event at 16:00 with notifications before \
80, 60, 55, 43 and 5 at 16:00 (in 43 minutes)"))

(org-wild-notifier-test notification-property-overriden
  "Tests that it is possible to customize the notification property"
  :time "15:29"
  :overrides ((org-wild-notifier-alert-times-property "NOTIFY_BEFORE"))
  :expected-alerts
  ("TODO event at 16:00 with NOTIFY_BEFORE property set to 31 at 16:00 (in 31 minutes)"))

(org-wild-notifier-test message-within-one-minute
  "Tests that message is correct"
  :time "14:59"
  :overrides ((org-wild-notifier-alert-time 1))
  :expected-alerts
  ("TODO event at 15:00 at 15:00 (in 1 minute)"))

(org-wild-notifier-test day-wide-events
  "Tests that user receives notifications on day-wide (w/o specified time) \
events"
  :time "14:59"
  :overrides ((org-wild-notifier--day-wide-events t))
  :expected-alerts
  ("TODO event scheduled on today at 00:00 (right now)"))

(org-wild-notifier-test next-day-events
  "Tests that user receives notifications on next day events"
  :time "23:50"
  :expected-alerts
  ("TODO event scheduled on tomorrow at 00:00 (in 10 minutes)"))

(org-wild-notifier-test keyword-whitelist
  "Tests that whitelist option filters out events."
  :time "15:50"
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 10 minutes)"
   "TODO event at 16:00 with NOTIFY_BEFORE property set to 31 at 16:00 (in 10 minutes)"
   "TODO event at 16:00 with notifications before 80, 60, 55, 43 and 5 at 16:00 (in 10 \
minutes)"
   "TODO event scheduled on 16:00 with deadline at 17:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test keyword-whitelist-disabled
  "Tests that whitelist option can be disabled"
  :time "15:50"
  :overrides ((org-wild-notifier-keyword-whitelist nil))
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 10 minutes)"
   "TODO event at 16:00 with NOTIFY_BEFORE property set to 31 at 16:00 (in 10 minutes)"
   "TODO event at 16:00 with notifications before 80, 60, 55, 43 and 5 at 16:00 (in 10 minutes)"
   "TODO event scheduled on 16:00 with deadline at 17:00 at 16:00 (in 10 minutes)"
   "Tagged event at 16:00 at 16:00 (in 10 minutes)"
   "Plain event at 16:00 at 16:00 (in 10 minutes)"
   "IN PROGRESS event at 16:00 at 16:00 (in 10 minutes)"
   "DONE event at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test keyword-whitelist-with-two-items
  "Tests that whitelist option can contain more than one items"
  :time "15:50"
  :overrides ((org-wild-notifier-keyword-whitelist '("IN PROGRESS" "DONE")))
  :expected-alerts
  ("IN PROGRESS event at 16:00 at 16:00 (in 10 minutes)"
   "DONE event at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test keyword-blacklist
  "Tests that blacklist option filters out events."
  :time "15:50"
  :overrides ((org-wild-notifier-keyword-whitelist '())
              (org-wild-notifier-keyword-blacklist '("TODO" "DONE")))
  :expected-alerts
  ("Tagged event at 16:00 at 16:00 (in 10 minutes)"
   "Plain event at 16:00 at 16:00 (in 10 minutes)"
   "IN PROGRESS event at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test tags-whitelist
  "Tests that whitelist option filters out events."
  :time "15:50"
  :overrides ((org-wild-notifier-tags-whitelist '("bar"))
              (org-wild-notifier-keyword-whitelist '()))
  :expected-alerts
  ("Tagged event at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test two-tags-whitelist
  "Tests that whitelist option filters out events."
  :time "15:50"
  :overrides ((org-wild-notifier-tags-whitelist '("bar" "baz"))
              (org-wild-notifier-keyword-whitelist '()))
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 10 minutes)"
   "Tagged event at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test mixed-whitelist
  "Tests that whitelist option filters out events."
  :time "15:50"
  :overrides ((org-wild-notifier-tags-whitelist '("bar" "baz"))
              (org-wild-notifier-keyword-whitelist '("IN PROGRESS")))
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 10 minutes)"
   "Tagged event at 16:00 at 16:00 (in 10 minutes)"
   "IN PROGRESS event at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test mixed-tags-whitelist-blacklist
  "Tests that blacklist option filters out events."
  :time "15:50"
  :overrides ((org-wild-notifier-keyword-whitelist '())
              (org-wild-notifier-tags-whitelist '("baz"))
              (org-wild-notifier-tags-blacklist '("foo")))
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test event-without-a-keyword
  "Tests that blacklist option filters out events."
  :time "19:25"
  :overrides ((org-wild-notifier-keyword-whitelist '()))
  :expected-alerts
  ("event without a keyword at 19:35 at 19:35 (in 10 minutes)"))

(org-wild-notifier-test non-existent-fixture
  "Tests that it doesn't hang if there's a non-existent agenda file."
  :fixture "bad.org"
  :expected-alerts ())
