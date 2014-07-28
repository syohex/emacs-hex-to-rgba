;;; test-hex-to-rgba.el --- unit test for hex-to-rgba.el

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

(require 'ert)
(require 'hex-to-rgba)

(ert-deftest convert ()
  "Convert hex string to RGBA list"
  (let ((got (hex-to-rgba--convert "ffffff")))
    (should (equal got '(255 255 255))))
  (let ((got (hex-to-rgba--convert "101010")))
    (should (equal got '(16 16 16))))
  (let ((got (hex-to-rgba--convert "111")))
    (should (equal got '(17 17 17)))))

(ert-deftest format ()
  "Format from hex representation to RGBA representation."
  (let ((got (hex-to-rgba--format '(255 255 255))))
    (should (string= got "rgba(255,255,255,1)")))
  (let ((got (hex-to-rgba--format '(0 0 0))))
    (should (string= got "rgba(0,0,0,1)"))))

(ert-deftest insert-at-first ()
  "Insert RGBA color representation at first point."
  (with-temp-buffer
    (insert "#FAFBFC")
    (goto-char (point-min))
    (call-interactively 'hex-to-rgba)
    (let ((got (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
      (should (string= got "rgba(250,251,252,1)")))))

(ert-deftest insert-at-middle ()
  "Insert RGBA color representation at middle point."
  (with-temp-buffer
    (insert "#FAFBFC")
    (backward-char 3)
    (call-interactively 'hex-to-rgba)
    (let ((got (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
      (should (string= got "rgba(250,251,252,1)")))))

(ert-deftest insert-at-end ()
  "Insert RGBA color representation at end point."
  (with-temp-buffer
    (insert "#FAFBFC")
    (call-interactively 'hex-to-rgba)
    (let ((got (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
      (should (string= got "rgba(250,251,252,1)")))))

;;; test-hex-to-rgba.el ends here
