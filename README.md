# Machine Learning for Hackers
## 概要
[Machine Learning for Hackers](http://www.amazon.co.jp/dp/4873115949)  
[著者様のサンプルコード](https://github.com/johnmyleswhite/ML_for_Hackers)  

一部、うまく動かなかった部分に軽く手を加えてる  

## データについて
リポジトリには含めない  
必要に応じて著者のリポジトリか、本家から取得する

なお、本家が記載されていないデータに関してはすべて著者のリポジトリからコピーして利用している

本家のダウンロード先は
* UFO
 * http://www.infochimps.com/datasets/60000-documented-ufo-sightings-with-text-descriptions-and-metada
* Spamメール
 * http://spamassassin.apache.org/publiccorpus/
 * ディレクトリ名を変更している
  * spam -> mail/spam_train
  * spam2 -> mail/spam_test
  * easyham -> mail/easyham_train
  * easyham2 -> mail/easyham_test
  * hardham -> mail/hardham_train
  * hardham -> mail/hardham_test
* 米国議員の点呼投票
 * ftp://voteview.com/junkord/SC01112D21_PRE_DATES.DTA
 * 第101-111議会を抜き出す必要あり

データはすべてdataディレクトリにダウンロードする前提となっている

## 動かし方
R Studio + knitr
