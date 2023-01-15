# 使い方

- ねこまど研スプレッドシートのA1から対局結果の一番右下のセルまでを別シートにコピーして、そのシートをcsvとして保存する
- 保存したcsvをresult.csvに改名して、elo_ratingディレクトリ(このREADMEが入っているディレクトリ)に置く
- ターミナルを開いて以下のコマンドを入力する
  - `cd ~/projects/elo_rating`
  - `ruby calculate.rb`
- elo_rating/archives/20xxxxxx/rating_result.csv(20xxxxxxの部分は今回の日付)を開く
- 内容をコピーして、スプレッドシートの適当な余白に貼り付ける
- 貼り付けた部分を選択して、"データ" -> "テキストを列に分割"
- 内容がセルに分かれるので、必要な部分をコピーして結果に貼り付ける

