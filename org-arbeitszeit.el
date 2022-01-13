;;; org-arbeitszeit.el --- Calculate your worktime               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  Benjamin Kästner <benjamin.kaestner@gmail.com>
;; URL: https://github.com/bkaestner/org-arbeitszeit
;; Keywords: tools, org, calendar, convenience
;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `org-arbeitszeit' provides a new dynamic block for Org called "arbeitszeit"
;; (German for "working hours").  The dynamic block collects all clocked hours
;; and provides a weekly table to quickly see your accumulated overtime.
;;
;; To insert a new block, use `M-x org-dynamic-block-insert-dblock'.
;;

;;; Todo:

;; `org-arbeitszeit' is currently limited to the current `org-mode' buffer.  I
;; already have a proof of concept at hand for the agenda files, but I believe
;; that the current `org-dblock-write:clocktable' can be inspected for more
;; better `:scope' handling in general. The invisioned `:scope' values are
;; `agenda', `agenda-with-archive', ("file1" ...), FUNCTION and `file' (will be
;; default).  Using the trees to accumulate working hours doesn't sound right.
;;
;; For holidays and vacations, I want to support org-references to lists and
;; tables. This is where the parameters :days-per-week and :hours-per-day will
;; come into play.

;;; Code:
(require 'cl-lib)
(require 'org-clock)

(defun org-arbeitszeit--warn-reserved (prop)
  "Warn about the usage of the reserved parameter PROP."
  (display-warning 'org-arbeitszeit
                   (format "the %s parameter is reserved for future use but currently not working" prop)
                   :warning))

(defun org-arbeitszeit--write-table (params)
  "Create the Arbeitszeittabelle using PARAMS.

PARAMS is a plist containg the following entries:

  :tstart (REQUIRED) - the start of your tracked time
  :tend   (REQUIRED) - the end of your tracked time
                       (must be greater than :tstart)
  :hours-per-day     - your working hours per day, default `8'
  :days-per-week     - your working days per week, default '5'
  :match             - see info node `(org) Matching tags and properties'

The parameters `:hours-per-day' and `:days-per-week' are used to calculate your
planned working time.  Currently, that might seem like a hassle, but will
hopefully make more sense when the `:holidays' and `:vacations' options are
implemented."
  (mapc #'org-arbeitszeit--warn-reserved
        (seq-intersection params '(:scope :files :holidays :vacations)))

  (let ((ts (plist-get params :tstart))
	(te (plist-get params :tend))
        (hours-per-day (or (plist-get params :hours-per-day) 8))
        (days-per-week (or (plist-get params :days-per-week) 5))
        (match (plist-get params :match)))
    
    (unless (and ts te)
      (error "Needs both :tstart and :tend set"))
    (when (string-match-p "<\\|>" ts)
      (setq ts (org-format-time-string "%Y-%m-%d" (org-matcher-time ts))))
    (when (string-match-p "<\\|>" te)
      (setq te (org-format-time-string "%Y-%m-%d" (org-matcher-time te))))
    (setq ts (org-date-to-gregorian ts))
    (setq te (org-date-to-gregorian te))

    (insert-before-markers "| Week | Hours | +Time |\n|-\n")

    (while (calendar-date-compare (list ts) (list te))
      (let ((week (org-format-time-string "%Y-W%0W" (org-time-from-absolute ts)))
            (nts  (list (car ts) (+ 7 (cadr ts)) (caddr ts)))
            (weektime 0))
        ;; Currently fixed to current file. I have another version that works among all
        ;; agenda files ready, but I'd like to provide some :scope support before I throw that
        ;; onto the user.
        (save-excursion
          (save-restriction
            (widen)
            (setq weektime (+ weektime (cadr (org-clock-get-table-data nil (list :block week :match match)))))))
        (insert (format "|%s|%s|\n" week (org-duration-from-minutes weektime 'h:mm)))
        (setq ts nts)))
    (insert-before-markers "|-\n|Total:|\n")
    (insert-before-markers (format "#+TBLFM: $3=$2-%s;U::@>$2..@>$>=vsum(@I..@II);U" (* 60 60 hours-per-day days-per-week) 'h:mm)))
  (forward-line -1)
  (org-table-recalculate t))

(defun org-dblock-write:arbeitszeit (params)
  "Write the standard Arbeitszeittabelle.

See `org-arbeitszeit--write-table' for a description of PARAMS."
  (interactive)
  (org-arbeitszeit--write-table params))

(defun org-arbeitszeit-in-arbeitszeittabelle-p ()
  "Check if the cursor is in a arbeitszeit block."
  (let ((pos (point)) start)
    (save-excursion
      (end-of-line 1)
      (and (re-search-backward "^[ \t]*#\\+BEGIN:[ \t]+arbeitszeit" nil t)
	   (setq start (match-beginning 0))
	   (re-search-forward "^[ \t]*#\\+END:.*" nil t)
	   (>= (match-end 0) pos)
	   start))))

(defun org-arbeitszeit-report ()
  "Update dynamic arbeitszeit block at point (or insert if there is no arbeitszeit at point)."
  (interactive)
  (pcase (org-arbeitszeit-in-arbeitszeittabelle-p)
    (`nil
     (org-create-dblock '(:tstart "<-4w>" :tend "<now>" :name "arbeitszeit")))
    (start (goto-char start)))
  (org-update-dblock))

(org-dynamic-block-define "arbeitszeit" #'org-arbeitszeit-report)

(provide 'org-arbeitszeit)
;;; org-arbeitszeit.el ends here
