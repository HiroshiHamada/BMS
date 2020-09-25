# 『社会科学のためのベイズ統計モデリング』サポートサイト

[『社会科学のためのベイズ統計モデリング』](https://www.amazon.co.jp/dp/4254128428/)
[（朝倉書店）](http://www.asakura.co.jp/books/isbn/978-4-254-12842-0/)
で解説した分析や計算を再現するための

1. データファイル
2. Rコード
3. Stanコード

を公開しています.

## 各章のディレクトリ構造

- 一番上の階層
  - Rコード，データファイル（csvファイルもしくはtxtファイル），PDFが置いてあります．
- modelフォルダ
  - stanコードが置いてあります．

## 実行方法

右上にある緑色のボタン（Clone or download）を押して，Download ZIPを選択してください．
全てのファイルがダウンロードできます．

分析を再現したい章のフォルダを開いて，Rコードを実行してください．

異なる章のコードを実行する際には，ホームディレクトリを変更してください．

## コード更新の記録

- ch04　ch-4.Rでrstanのヴァージョン変更に伴うエラーが生じたため修正しました（2020/02/12）
- ch11 ch-11.Rでrstanのヴァージョン変更に伴うエラーが生じたため修正しました（2020/02/12）
- ch07 作業ミスでファイルをアップしていなかったため，アップロードしました（2020/02/12）
- ch07 グラフを出力するコードを修正しました（2020/02/13）
- ch09 変数名amountのタイポを修正．stanコードへのパスを修正（2020/02/13）

コードの不具合をお知らせいただいた読者のみなさんに感謝します．
ありがとうございました．

## 補足情報

本書の補足資料です．スライドは浜田が授業用資料として作成したもので各章の要約です．ご自由にお使いください．

- BMS-ch01.pdf 　　第1章のスライドです
- BMS-ch02.pdf 　　第2章のスライドです
- BMS-ch03.pdf 　　第3章のスライドです
- BMS-ch04.pdf 　　第4章のスライドです
- BMS-ch05.pdf 　　第5章のスライドです
- BMS-ch06a.pdf 　　第6章前半のスライドです
- BMS-ch06b.pdf 　　第6章後半のスライドです
- KL_examples.pdf 　　KL情報量についてのノートです．黒木さんのコメント（[スレッドはこちら](https://twitter.com/genkuroki/status/1230047602852319232)）に感謝します．



## 正誤表

- errata.pdf 　　第2版までの修正箇所です．誤字や間違いをお知らせいただいた読者のみなさんに感謝します．
