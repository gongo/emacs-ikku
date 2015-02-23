emacs-ikku
====================

Discover haiku from text.

Requirements
--------------------

- Emacs 24
- [dash.el](https://github.com/magnars/dash.el)
- [MeCab](http://mecab.googlecode.com/svn/trunk/mecab/doc/index.html) with IPADIC (e.g. `brew install mecab mecab-ipadic`)

Usage
--------------------

### `ikku/find (text &optional rule)`

Return the first found song from given text.

```lisp
(require 'ikku)

(let ((song (ikku/find "ああ古池や蛙飛び込む水の音ああ")))
  (ikku:song-to-string song))

;; "古池や 蛙飛び込む 水の音"


(let ((song (ikku/find "ああ古池や蛙飛び込む水の音ああ" '(3 4 2))))
  (ikku:song-to-string song))

;; "蛙 飛び込む 水"
```

### `ikku/search (text &optional rule)`

Return all songs from given text.

```lisp
(dolist (song (ikku/search "ああ古池や蛙飛び込む水の音ああ天秤や京江戸かけて千代の春ああ"))
  (message (ikku:song-to-string song "/")))

;; 古池や/蛙飛び込む/水の音
;; 天秤や/京江戸かけて/千代の春
```

SEE ALSO
--------------------

- [r7kamura/ikku](https://github.com/r7kamura/ikku)

License
--------------------

MIT License
