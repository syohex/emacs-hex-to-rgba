;;; hex-to-rgba.el --- Hex to RGBA

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-hex-to-rgba
;; Version: 0.01
;; Package-Requires: ((cl-lib "0.5"))

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

(defun hex-to-rgba--range ()
  (save-excursion
    (unless (looking-at-p "#")
      (unless (search-backward "#" (line-beginning-position) t)
        (error "Not found hash mark of color hex")))
    (let ((re "#\\(?:[0-9a-fA-F]\\{3\\}\\(?:[0-9a-fA-F]\\{3\\}\\)?\\)"))
      (unless (re-search-forward re (line-end-position) t)
        (error "Not found color hex"))
      (cons (match-beginning 0) (match-end 0)))))

(defsubst hex-to-rgba-convert3 (hex-str)
  (cl-loop for c across hex-str
           for s = (char-to-string c)
           for val = (string-to-number s 16)
           collect (+ (* 16 val) val)))

(defsubst hex-to-rgba-convert6 (hex-str)
  (cl-loop for i from 0 below 6 by (+ i 2)
           for first = (string-to-number (char-to-string (aref hex-str i)) 16)
           for second = (string-to-number (char-to-string (aref hex-str (+ i 1))) 16)
           collect (+ (* 16 first) second)))

(defun hex-to-rgba--convert (hex-str)
  (if (= (length hex-str) 3)
      (hex-to-rgba-convert3 hex-str)
    (hex-to-rgba-convert6 hex-str)))

(defun hex-to-rgba--format (rgba)
  (format "rgba(%d,%d,%d,1)"
          (cl-first rgba)
          (cl-second rgba)
          (cl-third rgba)))

(defun hex-to-rgba--region (start end)
  (let* ((hex-str (buffer-substring-no-properties (1+ start) end))
         (rgba (hex-to-rgba--convert hex-str)))
    (goto-char start)
    (delete-region start end)
    (insert (hex-to-rgba--format rgba))))

;;;###autoload
(defun hex-to-rgba ()
  (interactive)
  (let ((range (hex-to-rgba--range)))
    (hex-to-rgba--region (car range) (cdr range))))

(provide 'hex-to-rgba)

;;; hex-to-rgba.el ends here
