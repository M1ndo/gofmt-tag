;;; gofmt-tag.el --- Format and align go struct tags -*- lexical-binding: t -*-

;; Written by ybenel (m1ndo) - 2023
;; Author: ybenel <http://github/m1ndo>
;; Maintainer: TEC <root@ybenel.cf>
;; Homepage: https://github.com/m1ndo/gofmt-tag
;; Version: 1.0.0
;; Keywords: format, align, structural, structures
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

;;; Code:

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
         (old-point (point)))
    (when file
      (write-region (point-min) (point-max) temp-file)
      (call-process gofmt-tag-executable nil output-buffer nil "-file" temp-file "-C")
      (with-current-buffer output-buffer
        (let* ((formatted-content (buffer-string))
               (current-md5sum (shell-command-to-string (concat "md5sum " temp-file " | awk '{ print $1 }'"))))
          (with-temp-file formatted-temp-file
            (insert formatted-content)
            (when (string-match "\n$" formatted-content)
              (delete-char -1)))
          (let* ((formatted-md5sum (shell-command-to-string (concat "md5sum " formatted-temp-file " | awk '{ print $1 }'"))))
            (if (string= current-md5sum formatted-md5sum)
                (message "Buffer is already formatted.")
              (message (format "Buffer is being formated? %s %s " current-md5sum formatted-md5sum))
              (progn
                (with-current-buffer (find-file-noselect file)
                  (erase-buffer)
                  (insert-file-contents formatted-temp-file)
                  (goto-char old-point))
                (message "Formatted with formattag."))))))
      (kill-buffer output-buffer)
      (delete-file formatted-temp-file))))


(define-minor-mode gofmt-tag-mode
  "Minor mode for aligning struct fields using formattag."
  :lighter "Gofmt-Tag is enabled."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c t") 'gofmt-tag)
            map))

;; Add it to go-mode hook
;; (add-hook 'go-mode-hook 'gofmt-tag-mode)

(provide 'gofmt-tag)

;;; gofmt-tag.el ends here
