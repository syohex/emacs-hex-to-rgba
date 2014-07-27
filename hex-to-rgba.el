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
        (error "Not found Color hex"))
      (cons (match-beginning 0) (match-end 0)))))

(defsubst hex-to-rgba-convert3 (hex-str)
  (cl-loop for c across hex-str
           for s = (char-to-string c)
           for val = (string-to-number s 16)
           collect (+ (* 16 val) val)))

(defsubst hex-to-rgba-convert6 (hex-str)
  (cl-loop repeat 3
           for i = 0 then (+ i 2)
           for first = (string-to-number (char-to-string (aref hex-str i)) 16)
           for second = (string-to-number (char-to-string (aref hex-str (+ i 1))) 16)
           collect (+ (* 16 first) second)))

(defun hex-to-rgba--convert (hex-str)
  (if (= (length hex-str) 3)
      (hex-to-rgba-convert3 hex-str)
    (hex-to-rgba-convert6 hex-str)))

(defun hex-to-rgba--format (rgba)
  (format "rgba(%d,%d,%d,%d)"
          (cl-first rgba)
          (cl-second rgba)
          (cl-third rgba)
          1))

;;;###autoload
(defun hex-to-rgba ()
  (interactive)
  (let* ((range (hex-to-rgba--range))
         (hex-str (buffer-substring-no-properties (1+ (car range)) (cdr range)))
         (rgba (hex-to-rgba--convert hex-str)))
    (save-excursion
      (goto-char (car range))
      (delete-region (car range) (cdr range))
      (insert (hex-to-rgba--format rgba)))))

(provide 'hex-to-rgba)

;;; hex-to-rgba.el ends here
