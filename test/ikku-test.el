(ert-deftest ikku/find--with-valid-song ()
  (let ((texts '("古池や蛙飛び込む水の音"
                 "ああ古池や蛙飛び込む水の音ああ"
                 "ああ古池や蛙飛び込む水の音ああ天秤や京江戸かけて千代の春ああ")))
    (dolist (text texts)
      (should (ikku:song-p (ikku/find text))))))

(ert-deftest ikku/find--with-invalid-song ()
  (let ((texts '(
                 "test"
                 "リビングでコーヒー飲んでだめになってる" ;; Ending with 連用タ接続 (だめになっ$)
                 "その人に金をあげたい人がいれば"         ;; Ending with 仮定形 (いれ$)
                 "学会に多分ネイティブほとんどいない"     ;; Ending with 未然形 (ほとんどい$)
                 "古池や蛙飛び込むかかったんだ"           ;; Ending with ん as 非自立名詞 (かかったん$)
                 )))
    (dolist (text texts)
      (should-not (ikku:song-p (ikku/find text))))))
