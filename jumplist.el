;;; jumplist.el --- Jump like vim jumplist

;; Copyright (C) 2015 ganmacs

;; Author: ganmacs <ganmacs_at_gmail.com>
;; Maintainer: ganmacs <ganmacs_at_gmail.com>
;; URL: https://github.com/ganmacs/jumplist
;; Version: 0.0.1
;; Keywords: jumplist vim

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

;;; Code:

(require 'cl-lib)

(defgroup jumplist nil
  "jumplist configuration options."
  :prefix "jumplist"
  :group 'jumplist)

(defcustom jumplist/max-length 100
  "Max length of jumplist."
  :type 'integer
  :group 'jumplist)

(defcustom jumplist/hook-commad-list '(end-of-buffer beginning-of-buffer find-file)
  "Max length of jumplist."
  :type 'list
  :group 'jumplist)

(defvar jumplist/list '()
  "Jumplist that save file info.")

(defvar jumplist/idx 0
  "Index of jumplist.")

(defvar jumplist/jumping nil
  "Jumplist state.")

(defun jumplist/do-jump (buff)
  "Do jump to target file and point from BUFF."
  (find-file (car buff))
  (goto-char (cdr buff)))

(defun jumplist/reset-idx ()
  "Reset `jumplist/idx'."
  (setq jumplist/idx 0))

(defun jumplist/last? ()
  "Check `jumplist/idx' is last of list."
  (= jumplist/idx (- (length jumplist/list) 1)))

(defun jumplist/first? ()
  "Check `jumplist/idx' is first of list."
  (= jumplist/idx 0))

(defun jumplist/dec-idx ()
  "Descrement `jumplist/idx'."
  (setq jumplist/idx (- jumplist/idx 1)))

(defun jumplist/inc-idx ()
  "Increment `jumplist/idx'."
  (setq jumplist/idx (+ jumplist/idx 1)))

(defun jumplist/drop! (idx)
  "Drop item form list of IDX."
  (nbutlast jumplist/list jumplist/idx))

(defun jumplist/push (pointer)
  "Push POINTER to `jumplist'."
  (while (> (length jumplist/list) jumplist/max-length)
    (nbutlast jumplist/list 1))
  (push pointer jumplist/list))

(defun jumplist/same-position? (pointer)
  (let ((new-point (cdr pointer))
        (top-point (cdar jumplist/list)))
    (cond ((not new-point) nil)
          ((not top-point) nil)
          ((eq (marker-position new-point) (marker-position top-point)) 't))))

(defun jumplist/set ()
  "The record data structure is (file-name . pointer)."
  (interactive)
  (if (buffer-file-name)
      (let ((pointer (cons (buffer-file-name) (point-marker))))
        (unless (jumplist/same-position? pointer)
          (when jumplist/jumping
            (jumplist/drop! jumplist/idx)
            (setq jumplist/jumping nil)
            (jumplist/reset-idx))
          (jumplist/push pointer)))))

(defun jumplist/do-command? (command blacklist)
  (if blacklist
      (or
       (eq command (car blacklist))
       (jumplist/do-command? command (cdr blacklist)))))

(defun jumplist/commad-hook ()
  "Pre commad hook that call `jumplist/set' when registerd command hook called."
  (if (jumplist/do-command? this-command jumplist/hook-commad-list)
      (jumplist/set)))
(add-hook 'pre-command-hook 'jumplist/commad-hook)

;;;###autoload
(defun jumplist/previous-jump ()
  "Jump back."
  (interactive)
  (if (and (not (jumplist/first?))
           (jumplist/last?))
      (message "No further undo point.")
    (unless jumplist/jumping
      (jumplist/set)
      (setq jumplist/jumping 't))
    (jumplist/inc-idx)
    (let ((buff (nth jumplist/idx jumplist/list)))
      (jumplist/do-jump buff))))

;;;###autoload
(defun jumplist/forward-jump ()
  "Jump forward."
  (interactive)
  (if (jumplist/first?)
      (message "No further redo point.")
    (unless jumplist/jumping
      (jumplist/set)
      (setq jumplist/jumping 't))
    (jumplist/dec-idx)
    (let ((buff (nth jumplist/idx jumplist/list)))
      (jumplist/do-jump buff))))

(provide 'jumplist)
;;; jumplist.el ends here
