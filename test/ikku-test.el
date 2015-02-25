(ert-deftest ikku/find--with-valid-song ()
  (let ((texts '("古池や蛙飛び込む水の音"
                 "ああ古池や蛙飛び込む水の音ああ"
                 "ああ古池や蛙飛び込む水の音ああ天秤や京江戸かけて千代の春ああ")))
    (dolist (text texts)
      (should (ikku:song-p (ikku/find text))))))

(ert-deftest ikku/find--with-invalid-song ()
  (let ((texts '(
                 "test"
                 "リビングでコーヒー飲んでだめになってる" ;; Song ending with 連用タ接続 (だめになっ$)
                 "その人に金をあげたい人がいれば"         ;; Song ending with 仮定形 (いれ$)
                 "学会に多分ネイティブほとんどいない"     ;; Song ending with 未然形 (ほとんどい$)
                 "古池や蛙飛び込むかかったんだ"           ;; Song ending with ん as 非自立名詞 (かかったん$)
                 "新宿の桜と庭の写真撮っ"                 ;; Song ending with 連用タ接続 (撮っ$)
                 "炊きつけて画面眺めて満足し"             ;; Song ending with サ変・スル in 連用形 (-し$)
                 "レバーのお汁飲んだので元気出た"         ;; Phrase ending with 接頭詞
                 )))
    (dolist (text texts)
      (should-not (ikku:song-p (ikku/find text))))))

(ert-deftest ikku/judge--with-invalid-song ()
  (let ((texts '(
                 "ああ古池や蛙飛び込む水の音ああ"
                 "、古池や蛙飛び込む水の音"       ;; Phrase starting with symbol
                 )))
    (dolist (text texts)
      (should-not (ikku/judge text)))))
