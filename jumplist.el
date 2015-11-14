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

(defcustom jumplist/ignored-file-patterns '("COMMIT_EDITMSG$" "TAGS$")
  "Ignore file patter."
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

;; (setq jumplist/list (cdr jumplist/list))

;; (defun jumplist/do-jump-with-idx (buff idx)
;;   "BUFF and IDX."
;;   ())


(defun jumplist/reset-idx ()
  (setq jumplist/idx -0))

(defun jumplist/inc-idx ()
  (when jumplist/jumping
    (setq jumplist/idx
          (+ jumplist/idx
             (if (= 1 jumplist/jumping)         ; previous
                 1
               2)))
    (if (< (- jumplist/max-length 1) jumplist/jumping)
        (setq jumplist/jumping (- jumplist/max-length 1)))))

(defun jumplist/dec-idx ()
  (when jumplist/jumping
    (setq jumplist/idx
          (- jumplist/idx
             (if (= 2 jumplist/jumping)         ; forward
                 1
               2)))
    (if (> 0 jumplist/jumping) (setq jumplist/jumping 0))))

(defun jumplist/jump-previous ()
  "Undo Jump."
  (interactive)
  (if (and jumplist/list
           (< jumplist/idx (length jumplist/list))
           (< jumplist/idx (- jumplist/max-length 1)))
      (let ((buff (if jumplist/jumping
                      (nth jumplist/idx jumplist/list)
                    (car jumplist/list))))
        (jumplist/do-jump buff)
        (jumplist/inc-idx)
        (setq jumplist/jumping 1)       ; 1 is previous
        ;; (recenter)
        )
    (message "Undo list is empty")))

(defun jumplist/jump-forward ()
  "Redo Jump."
  (interactive)
  (if (and jumplist/list (< 0 jumplist/idx) jumplist/jumping)
      (let ((buff (nth (if (= (length jumplist/list) jumplist/idx) ;last

                           jumplist/idx) jumplist/list)))
        (jumplist/do-jump buff)
        (jumplist/dec-idx)
        (setq jumplist/jumping 2)       ; 2 is forward
        ;; (recenter)
        ))
  (message "Redo list is empty"))

(defun jumplist/push (pointer)
  "Push POINTER to `jumplist'."
  (while (> (length jumplist/list) jumplist/max-length)
    (nbutlast jumplist/list 1))
  (push pointer jumplist/list))

(defun jumplist/drop! (idx)
  (nbutlast jumplist/list jumplist/idx))

(defun jumplist/set ()
  "The record data structure is (file-name . pointer)."
  (interactive)
  (let ((pointer (cons (buffer-file-name) (point-marker))))
    (when jumplist/jumping
      (jumplist/drop! jumplist/idx)
      (setq jumplist/jumping nil)
      (jumplist/reset-idx))
    (jumplist/push pointer)))

(provide 'jumplist)
;;; jumplist.el ends here

(defun jumplist/last? ()
  (= jumplist/idx (- (length jumplist/list) 1)))

(defun jumplist/first? ()
  (= jumplist/idx 0))

(defun jumplist/dec-idx ()
  (setq jumplist/idx (- jumplist/idx 1)))

(defun jumplist/inc-idx ()
  (setq jumplist/idx (+ jumplist/idx 1)))

(defun jumplist/validate-inc-idx ()
  (when (jumplist/last?) (error "last!")))

(defun jumplist/validate-dec-idx ()
  (when (jumplist/first?) (error "first!")))


(defun jumplist/do-jump (buff)
  "Do jump to target file and point from BUFF."
  (find-file (car buff))
  (goto-char (cdr buff)))

(defun jumplist/prev-jump ()
  "Jump."
  (interactive)
  (progn
    (jumplist/validate-inc-idx)
    (unless jumplist/jumping
      (jumplist/set)
      (setq jumplist/jumping 't))
    (jumplist/inc-idx))
  (let ((buff (nth jumplist/idx jumplist/list)))
    (jumplist/do-jump buff)))

(defun jumplist/next-jump ()
  "Jump."
  (interactive)
  (progn
    (jumplist/validate-dec-idx)
    (unless jumplist/jumping
      (jumplist/set)
      (setq jumplist/jumping 't))
    (jumplist/dec-idx))
  (let ((buff (nth jumplist/idx jumplist/list)))
    (jumplist/do-jump buff)))

(global-set-key (kbd "s-j") 'jumplist/set)
(global-set-key (kbd "s-g") 'jumplist/prev-jump)
(global-set-key (kbd "s-G") 'jumplist/next-jump)

(progn
  (setq jumplist/jumping nil)
  (jumplist/reset-idx)
  (setq jumplist/list '()))
