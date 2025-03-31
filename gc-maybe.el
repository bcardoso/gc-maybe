;;; gc-maybe.el --- GC Maybe Trick -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/gc-maybe
;; Version: 0.2
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; GC Maybe Trick.


;;; Code:

(defgroup gc-maybe nil
  "Group for `gc-maybe' customizations."
  :group 'alloc)

(defcustom gc-maybe-threshold-max (* 512 1024 1024)
  "Maximum number of bytes of consing between garbage collections."
  :type 'integer)

(defcustom gc-maybe-percentage-max 0.6
  "Maximum portion of the heap used for allocation."
  :type 'float)

(defcustom gc-maybe-threshold-default gc-cons-threshold
  "Number of bytes of consing between garbage collections."
  :type 'integer)

(defcustom gc-maybe-percentage-default gc-cons-percentage
  "Portion of the heap used for allocation."
  :type 'float)

(defcustom gc-maybe-mode-idle 5
  "Idle seconds to run `gc-maybe' when `gc-maybe-mode' is on."
  :type 'integer)

(defcustom gc-maybe-restore-idle 3
  "Idle seconds to restore GC raised by `gc-maybe-raise-threshold-briefly'."
  :type 'integer)

(defcustom gc-maybe-log-stats nil
  "Log GC stats in `gc-maybe-log-buffer'."
  :type 'boolean)

(defcustom gc-maybe-log-buffer "*gc-log*"
  "GC log buffer."
  :type 'string)

(defun gc-maybe-raise-threshold (&rest _)
  "Raise GC threshold."
  (setq gc-cons-threshold  gc-maybe-threshold-max)
  (setq gc-cons-percentage gc-maybe-percentage-max))

(defun gc-maybe-raise-threshold-briefly (&rest _)
  "Raise GC threshold briefly.
Restore it after `gc-maybe-restore-idle' seconds."
  (gc-maybe-raise-threshold)
  (cancel-function-timers #'gc-maybe-restore-threshold)
  (run-with-idle-timer gc-maybe-restore-idle nil
                       #'gc-maybe-restore-threshold))

(defun gc-maybe-restore-threshold (&rest _)
  "Restore GC to saner values."
  (setq gc-cons-threshold  gc-maybe-threshold-default)
  (setq gc-cons-percentage gc-maybe-percentage-default))

(defun gc-maybe ()
  "Maybe GC.
Run `garbage-collect-maybe' with factor as 1/`gc-cons-percentage'."
  (and (garbage-collect-maybe (round (/ 1 gc-cons-percentage)))
       gc-maybe-log-stats
       (gc-maybe-log)))

(defmacro gc-maybe-with-buffer (&rest body)
  "Insert BODY in `gc-maybe-log-buffer'."
  (declare (indent defun))
  `(with-current-buffer (get-buffer-create gc-maybe-log-buffer)
     (special-mode)
     (setq-local buffer-read-only nil)
     (goto-char (point-max))
     ,@body
     (setq-local buffer-read-only t)))

(defvar gc-maybe-log--last-elapsed gc-elapsed
  "Last GC elapsed time.")

(defun gc-maybe-log-average ()
  "Return the average GC time."
  (condition-case nil
      (/ gc-elapsed gcs-done)
    (arith-error gc-elapsed)))

(defun gc-maybe-log ()
  "Log GC statistics."
  (when gc-maybe-log-stats
    (gc-maybe-with-buffer
      (insert
       (concat (format-time-string "[%F %T] " (current-time))
               (format "GC took %.3fs, average is %.3fs in %s GCs\n"
                       (- gc-elapsed (or gc-maybe-log--last-elapsed 0))
                       (gc-maybe-log-average)
                       gcs-done))))
    (setq gc-maybe-log--last-elapsed gc-elapsed)))

;;;###autoload
(defun gc-maybe-log-current ()
  "Print current GC statistics."
  (interactive)
  (let ((msg (format
              "[GC] Threshold: %s / Percentage: %s / Avg: %.3fs / GCs: %s"
              (file-size-human-readable gc-cons-threshold 'iec " ")
              gc-cons-percentage
              (gc-maybe-log-average)
              gcs-done)))
    (gc-maybe-with-buffer
      (insert (concat "-----\n" msg "\n-----\n")))
    (message msg)))

;;;###autoload
(defun gc-maybe-log-usage ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (let ((mem (garbage-collect))
        (separator "-----"))
    (gc-maybe-with-buffer
      (insert separator)
      (insert
       (concat
        (format "\n\n%-14s %-10s %-10s %-10s\n" 'TYPE 'USED 'FREE 'TOTAL)
        (make-string 50 ?-)))
      (mapc
       (lambda (i)
         (let ((type (nth 0 i)) (size (nth 1 i))
               (used (nth 2 i)) (free (nth 3 i)))
           (insert
            (format
             "\n%-14s %-10s %-10s %-10s"
             type
             (file-size-human-readable (* used size) 'iec " ")
             (file-size-human-readable (* (or free 0) size) 'iec " ")
             (file-size-human-readable (+ (* used size)
                                          (* (or free 0) size))
                                       'iec " ")))))
       mem)
      (insert (concat "\n\n" separator "\n"))))
  (pop-to-buffer gc-maybe-log-buffer)
  (recenter))

;;;###autoload
(define-minor-mode gc-maybe-mode
  "Minor mode for GC strategy."
  :global t
  :lighter nil
  (if gc-maybe-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'gc-maybe-raise-threshold -90)
        (add-hook 'minibuffer-exit-hook  #'gc-maybe-restore-threshold 90)
        (run-with-idle-timer gc-maybe-mode-idle t #'gc-maybe))
    (remove-hook 'minibuffer-setup-hook #'gc-maybe-raise-threshold)
    (remove-hook 'minibuffer-exit-hook  #'gc-maybe-restore-threshold)
    (cancel-function-timers #'gc-maybe)))


(provide 'gc-maybe)

;;; gc-maybe.el ends here
