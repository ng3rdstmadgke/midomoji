# 説明
形態素解析を行うプログラムです。
辞書は[mecab-ipadic](http://taku910.github.io/mecab/#download)を使います。

# クイックスタート
### 1. sbtのインストール
https://www.scala-sbt.org/1.x/docs/ja/Setup.html
### 2. midomojiのインストール
midomojiをダウンロード
```
git clone https://github.com/ng3rdstmadgke/midomoji.git
cd midomoji
```
ipadicをダウンロード
```
wget -O ipadic.tar.gz 'https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7MWVlSDBCSXZMTXM'
tar xvzf ipadic.tar.gz
```
midomojiをインストール
```
make ipadic=./mecab-ipadic-2.7.0-20070801
```
解析
```
echo "すもももももももものうち" | java -jar target/scala-2.12/midomoji.jar analyze -f detail
```

