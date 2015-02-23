;;; flycheck-ikku.el --- Flycheck: Ikku support

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/emacs-ikku
;; Version: 0.0.1
;; Keywords: flycheck haiku ikku text

;; Copyright (c) 2015 Wataru MIYAGUNI
;;
;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(require 'flycheck)
(require 'ikku)

;; example text:
;;
;; ピザが食べたい
;; 一句なう
;;
;; ほげええ
;;
;;----
;; result mecab:
;;
;; ピザ
;; が
;; 食べ
;; たい
;; EOS
;; 一句
;; なう
;; EOS
;; EOS
;; ほ
;; げ
;; え
;; え
;; EOS
;;
;;----
;; result of `flycheck-ikku/parse--as-multiline'
;;
;; (([cl-struct-ikku:node "ピザ"] [cl-struct-ikku:node "が"] [cl-struct-ikku:node "食べ"] [cl-struct-ikku:node "たい"])
;;  ([cl-struct-ikku:node "EOS"] )
;;  ([cl-struct-ikku:node "一句"] [cl-struct-ikku:node "なう"] )
;;  ([cl-struct-ikku:node "EOS"]  [cl-struct-ikku:node "EOS"]  )
;;  ([cl-struct-ikku:node "ほげ"] [cl-struct-ikku:node "げ"] [cl-struct-ikku:node "え"] [cl-struct-ikku:node "え"]))
;;
;; EOS is one: return code before line
;; EOS is two or more: return code before line + empty line
;;
(defun flycheck-ikku/parse--as-multiline (text)
  (let ((nodes (ikku/parse text)))
    (-partition-by 'ikku:node--eos-p nodes)))

(defun ikku-checker-start (checker callback)
  (let ((errors ())
        (rule '(5 7 5))
        (text (buffer-substring-no-properties (point-min) (point-max)))
        (current-line 0))
    (dolist (line-nodes (flycheck-ikku/parse--as-multiline text))
      (if (ikku:node--eos-p (car line-nodes))
          ;; This node is return code and any empty lines
          (setq current-line (+ current-line (1- (length line-nodes))))
        (progn
          (let ((position-bol (ikku:node-position (car line-nodes)))
                song)
            (setq song (ikku/find--nodes-to-song line-nodes rule))
            (setq current-line (1+ current-line))
            (when song
              (push (flycheck-error-new-at current-line
                                           (1+ (- (ikku:node-position (caar (ikku:song-phrases song))) position-bol))
                                           'warning
                                           (concat "一句じゃん: " (ikku:song-to-string song))
                                           :checker checker)
                    errors))))))
    (funcall callback 'finished errors)))

(flycheck-define-generic-checker 'ikku-checker
  "A ikku checker"
  :start #'ikku-checker-start
  :predicate (lambda () t))

;; (add-to-list 'flycheck-checkers 'ikku-checker)
