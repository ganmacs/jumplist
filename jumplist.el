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

(defun build-pointer-info ()
  "Build infomation of file that are point fullpath and buffername."
  (list
   (cons 'point (point-marker))
   (cons 'full-path (buffer-file-name))
   (cons 'buffer-name (buffer-name))))

(defun get-info (key pointer)
  "Get info by KEY from POINTER."
  (cdr (assoc key pointer)))

(defun jumplist/jump ()
  "Jump."
  (when jumplist/list
    (let ((pointer (car jumplist/list)))
      (find-file (get-info 'full-path pointer))
      (goto-char (get-info 'point pointer))
      ;; (setq jumplist/list (cdr jumplist/list))
      (recenter))))

(defun jumplist/push (pointer)
  "Push POINTER to `jumplist'."
  ;; (while (> (length jumplist/list) jumplist/max-length)
  ;;   (nbutlast jumplist/list 1))
  (add-to-list 'jumplist/list pointer))

(defun jumplist/set ()
  (let ((pointer (build-pointer-info)))
    (jumplist/push pointer)))


(provide 'jumplist)

;;; jumplist.el ends here
