;;; gofmt-tag.el --- Format and align go struct tags -*- lexical-binding: t -*-

;; Written by ybenel (m1ndo) - 2023
;; Author: ybenel <http://github/m1ndo>
;; Maintainer: ybenel <root@ybenel.cf>
;; Homepage: https://github.com/m1ndo/gofmt-tag
;; Version: 1.0.1
;; Keywords: tools, wp, matching
;; Package-Requires: ((emacs "27"))

;; This file is not part of GNU Emacs.
;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A package for formatting struct tags in golang (go-mode)
;; Requires `formattag' in order to work,
;; Add it to go-mode hook with `(add-hook 'go-mode-hook 'gofmt-tag-mode)`

;;; Code:
(require 'cl-lib)

(defvar gofmt-tag-executable "formattag"
  "The path to the formattag executable.")

(defun gofmt-tag ()
  "Format the current buffer using gofmt-tag if it is not already formatted."
  (interactive)
  (unless (executable-find gofmt-tag-executable)
    (message "formattag executable not found.")
    (cl-return))
  (let* ((temp-file (make-temp-file "gofmttag"))
         (formatted-temp-file (make-temp-file "formatted-"))
         (output-buffer (generate-new-buffer "*gofmt-tag-output*"))
         (file (buffer-file-name))
         (current-md5sum (secure-hash 'md5 (current-buffer)))
         (old-point (point)))
    (when file
      (write-region (point-min) (point-max) temp-file)
      (call-process gofmt-tag-executable nil output-buffer nil "-file" temp-file "-C")
      (with-current-buffer output-buffer
        (let ((formatted-content (buffer-string)))
          (with-temp-file formatted-temp-file
            (insert formatted-content)
            (when (string-match "\n$" formatted-content)
              (delete-char -1)))
          (let* ((formatted-md5sum (secure-hash 'md5 (find-file-noselect formatted-temp-file))))
            (if (string= current-md5sum formatted-md5sum)
                (message "Buffer is already formatted.")
              (progn
                (with-current-buffer (find-file-noselect file)
                  (erase-buffer)
                  (insert-file-contents formatted-temp-file)
                  (goto-char old-point))
                (message "Formatted with formattag."))))))
      (kill-buffer output-buffer)
      (delete-file formatted-temp-file)
      (kill-buffer formatted-temp-file))))


(define-minor-mode gofmt-tag-mode
  "Minor mode for aligning struct fields using formattag."
  :lighter " GoFmt-Tag"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-t") 'gofmt-tag)
            map))

(provide 'gofmt-tag)

;;; gofmt-tag.el ends here
