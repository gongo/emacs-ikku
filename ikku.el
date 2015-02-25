;;; ikku.el --- Discover haiku from text

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/emacs-ikku
;; Version: 0.0.1
;; Keywords: haiku ikku text

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

(require 'dash)
(require 'cl-lib)

(cl-defstruct (ikku:node
               (:constructor new-ikku:node
                             (surface stat position features &aux
                                      (stat     (string-to-number stat))
                                      (features (split-string features ","))
                                      (type          (nth 0 features))
                                      (subtype1      (nth 1 features))
                                      (subtype2      (nth 2 features))
                                      (subtype3      (nth 3 features))
                                      (conjugation1  (nth 4 features))
                                      (conjugation2  (nth 5 features))
                                      (root-form     (nth 6 features))
                                      (pronunciation (nth 8 features)))))
  surface
  stat
  position
  features
  type
  subtype1
  subtype2
  subtype3
  conjugation1
  conjugation2
  root-form
  pronunciation)

(cl-defstruct (ikku:song
               (:constructor new-ikku:song (nodes rule)))
  phrases
  rule)

(cl-defstruct ikku:scanner
  nodes
  rule
  current-count)

(defun ikku:node--normal-p (node)
  (eq (ikku:node-stat node) 0))

(defun ikku:node--eos-p (node)
  (eq (ikku:node-stat node) 3))

(defun ikku:node--first-of-phrase-p (node)
  (cond ((-contains-p '("助詞" "助動詞") (ikku:node-type node))
         nil)
        ((-contains-p '("非自立" "接尾") (ikku:node-subtype1 node))
         nil)
        ((and
          (string= "自立" (ikku:node-subtype1 node))
          (-contains-p '("する" "できる") (ikku:node-root-form node)))
         nil)
        (t t)))

(defun ikku:node--first-of-ikku-p (node)
  (cond ((not (ikku:node--first-of-phrase-p node))
         nil)
        ((and
          (string= (ikku:node-type node) "記号")
          (not (-contains-p '("括弧開" "括弧閉") (ikku:node-subtype1 node))))
         nil)
        (t t)))

(defun ikku:node--last-of-phrase-p (node)
  (not (string= "接頭詞" (ikku:node-type node))))

(defun ikku:node--last-of-ikku-p (node)
  (cond ((-contains-p '("名詞接続" "格助詞" "係助詞" "連体化" "接続助詞" "並立助詞" "副詞化" "数接続" "連体詞") (ikku:node-type node))
         nil)
        ((string= (ikku:node-conjugation2 node) "連用タ接続")
         nil)
        ((and (string= (ikku:node-conjugation1 node) "サ変・スル")
              (string= (ikku:node-conjugation2 node) "連用形"))
         nil)
        ((and (string= (ikku:node-type node) "動詞")
              (-contains-p '("仮定形" "未然形") (ikku:node-conjugation2 node)))
         nil)
        ((and (string= (ikku:node-type node) "名詞")
              (string= (ikku:node-subtype1 node) "非自立")
              (string= (ikku:node-pronunciation node) "ン"))
         nil)
        (t t)))

(defun ikku:node--pronunciation-length (node)
  (let ((pronunciation (ikku:node-pronunciation node)))
    (if pronunciation
        (length (replace-regexp-in-string "[^アイウエオカ-モヤユヨラ-ロワヲンヴー]" "" pronunciation))
      0)))

(defun ikku:scanner--node-consume-p (scanner node)
  (let ((max-consumable-length (ikku:scanner--max-consumable-length scanner))
        (pronunciation-length (ikku:node--pronunciation-length node)))
    (cond ((> pronunciation-length max-consumable-length)
           nil)
          ((not (ikku:node--normal-p node))
           nil)
          ((and (ikku:scanner--first-of-phrase-p scanner)
                (not (ikku:node--first-of-phrase-p node)))
           nil)
          ((and (eq pronunciation-length max-consumable-length)
                (not (ikku:node--last-of-phrase-p node)))
           nil)
          (t t))))

(defun ikku:scanner--first-of-phrase-p (scanner)
  (let ((rule (ikku:scanner-rule scanner))
        (count (ikku:scanner-current-count scanner)))
    (-contains-p
     (-reduce-from (lambda (memo item)
                     (-snoc memo (+ (-last-item memo) item)))
                   '(0)
                   rule)
     count)))

(defun ikku:scanner--has-full-count-p (scanner)
  (let ((rule (ikku:scanner-rule scanner))
        (count (ikku:scanner-current-count scanner)))
    (eq count (-sum rule))))

(defun ikku:scanner--has-valid-first-node-p (scanner)
  (ikku:node--first-of-ikku-p (-first-item (ikku:scanner-nodes scanner))))

(defun ikku:scanner--has-valid-last-node-p (scanner phrases)
  (ikku:node--last-of-ikku-p (-last-item (-last-item phrases))))

(defun ikku:scanner--max-consumable-length (scanner)
  (let ((rule (ikku:scanner-rule scanner))
        (count (ikku:scanner-current-count scanner))
        (phrase-index (ikku:scanner--phrase-index scanner)))
    (- (-sum (-take (1+ phrase-index) rule)) count)))

(defun ikku:scanner--phrase-index (scanner)
  (let ((rule (ikku:scanner-rule scanner))
        (count (ikku:scanner-current-count scanner)))
    (1- (or (--first
             (< count (-sum (-slice rule 0 it)))
             (-iterate '1+ 1 (length rule)))
            (length rule)))))

(defun ikku:scanner--satisfied-p (scanner phrases)
  (and (ikku:scanner--has-full-count-p scanner)
       (ikku:scanner--has-valid-last-node-p scanner phrases)))

(defun ikku:scanner--scan (scanner)
  (let* ((rule (ikku:scanner-rule scanner))
         (phrases (make-list (length rule) '())))
    (when (ikku:scanner--has-valid-first-node-p scanner)
      (cl-block scan
        (dolist (node (ikku:scanner-nodes scanner))
          (if (not (ikku:scanner--node-consume-p scanner node))
              (cl-return-from scan)
            (progn
              (setf (nth (ikku:scanner--phrase-index scanner) phrases)
                    (-snoc (nth (ikku:scanner--phrase-index scanner) phrases) node))
              (setf (ikku:scanner-current-count scanner)
                    (+ (ikku:scanner-current-count scanner)
                       (ikku:node--pronunciation-length node)))
              (when (ikku:scanner--satisfied-p scanner phrases)
                (cl-return-from scan phrases)))))
        (when (ikku:scanner--satisfied-p scanner phrases)
          phrases)))))

(defun ikku:song-to-string (song &optional sep)
  (when (null sep) (setq sep " "))
  (mapconcat (lambda (phrase) (mapconcat 'ikku:node-surface phrase ""))
             (ikku:song-phrases song)
             sep))

(defun ikku/parse (text)
  (with-temp-buffer
    (let ((command (format "echo %s | mecab --node-format='%%m\\t%%s\\t%%H\\n' --eos-format='EOS\\t%%s\\t%%H\\n'" (shell-quote-argument text)))
          (position 0)
          node)
      (unless (zerop (call-process-shell-command command nil t))
        (error "Failed mecab"))
      (goto-char (point-min))

      (cl-loop while (re-search-forward "^\\([^\t]+\\)\t\\([0-9]\\)\t\\(.+\\)$" nil t)
               collect (prog1
                           (setq node (new-ikku:node (match-string 1) (match-string 2) position (match-string 3)))
                         (setq position (+ position (length (ikku:node-surface node)))))))))

(defun ikku/scan (nodes rule)
  (let ((scanner (make-ikku:scanner :nodes nodes :rule rule :current-count 0)))
    (ikku:scanner--scan scanner)))

(defun ikku/find--nodes-to-song (nodes rule &optional exactly)
  (cl-loop for n on nodes with phrases = nil
           do (setq phrases (ikku/scan n rule))
           when phrases return (make-ikku:song :phrases phrases :rule rule)
           when exactly return nil))

(defun ikku/find (text &optional rule)
  (when (not rule) (setq rule '(5 7 5)))
  (ikku/find--nodes-to-song (ikku/parse text) rule))

(defun ikku/judge (text &optional rule)
  (when (not rule) (setq rule '(5 7 5)))
  (ikku:song-p (ikku/find--nodes-to-song (ikku/parse text) rule t)))

(defun ikku/search (text &optional rule)
  (when (not rule) (setq rule '(5 7 5)))
  (cl-loop for n on (ikku/parse text) with phrases = nil
           do (setq phrases (ikku/scan n rule))
           when phrases collect (make-ikku:song :phrases phrases :rule rule)))

(provide 'ikku)
