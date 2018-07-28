package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;
import scala.io.StdIn.readLine;
import scala.collection.mutable.{ListBuffer, HashMap};
import java.nio.file.{Paths, Files};

object Main {

  
  def main(args: Array[String]): Unit = {
    args.toList match {
      // -b ./dictionary/matrix.def ./dictionary/morpheme.csv ./dictionary/dictionary_set.bin
      case ("-b" | "--build") :: mtPath :: ptPath :: dict :: xs => {
        // 連接コスト表の作成
        val connCost = Using[Source, Matrix](Source.fromFile(mtPath)) { s =>
          // 1行目はコスト表の縦と横の大きさがスペース区切りで入っている。
          val iter = s.getLines;
          val (leftSize, rightSize) = if (iter.hasNext) Util.parseMTFirst(iter.next()) else (0, 0);
          val mt = Matrix(leftSize, rightSize);
          iter.foreach { line =>
            Util.parseMT(line) match {
              case None            => ();
              case Some((l, r, c)) => mt.setCost(l, r, c);
            }
          }
          mt;
        }

        // 形態素辞書の作成
        // 表層形,左文脈ID,右文脈ID,コスト,品詞,品詞細分類1,品詞細分類2,品詞細分類3,活用型,活用形,原形,読み,発音
        val prefixTree = Using[Source, PrefixTree[Morpheme]](Source.fromFile(ptPath)) { s =>
          val pt = PrefixTree[Morpheme](500000);
          s.getLines.foreach { line =>
            Util.parsePT(line) match {
              case None           => ();
              case Some(morpheme) => pt.add(morpheme.surface, morpheme);
            }
          }
          pt;
        }
        DictionarySet(prefixTree, connCost).serialize(dict);
      }

      // -cm ./dictionary/dictionary_set.bin ./dictionary/matrix.def
      case ("-cm" | "--check-matrix") :: dict :: mtPath :: xs => {
        // Matrix オブジェクトのテスト
        val dictSet = DictionarySet(dict);
        val mt = dictSet.matrix;
        val checkMatrix: Source => Boolean = s => {
          s.getLines.foldLeft(true) { (ret, line) =>
            Util.parseMT(line) match {
              case None            => ret;
              case Some((l, r, c)) => {
                if (c == mt.getCost(l, r)) {
                  ret;
                } else {
                  println("left=%d, right=%d, cost=%d, dictCost=%d".format(l, r, c, mt.getCost(l,r)));
                  false;
                }
              }
            }
          }
        }
        // 正しく登録されているかチェック
        if (Using[Source, Boolean](Source.fromFile(mtPath))(checkMatrix)) println("OK!!") else println("NG...");
      }

      // -cp ./dictionary/dictionary_set.bin ./dictionary/morpheme.csv
      case ("-cp" | "--check-prefixtree") :: dict :: ptPath :: xs => {
        // Matrix オブジェクトのテスト
        val dictSet = DictionarySet(dict);
        val pt = dictSet.prefixtree;
        val checkPT: Source => Boolean = s => {
        // 表層形,左文脈ID,右文脈ID,コスト,品詞,品詞細分類1,品詞細分類2,品詞細分類3,活用型,活用形,原形,読み,発音
          s.getLines.foldLeft(true) { (ret, line) =>
            Util.parsePT(line) match {
              case None           => ret;
              case Some(morpheme) => {
                val exists = pt.find(morpheme.surface) match {
                  case None     => false;
                  case Some(ms) => ms.exists(m => if (morpheme == m) true else false);
                }
                if (exists) ret else false;
              }
            }
          }
        }
        // 正しく登録されているかチェック
        if (Using[Source, Boolean](Source.fromFile(ptPath))(checkPT)) println("OK!!") else println("NG...");
      }
      case ("-d" | "--debug") :: xs => {
        debug();
      }
      case ("-a" | "--analysis") :: xs  => {
        println("解析");
      }
      case _ => {
        val help = ListBuffer[String]();
        help += "-b, --build [MATRIX] [PREFIXTREE_PATH] [DICT]     : 辞書の構築";
        help += "-d, --debug                                                    : デバッグモード起動";
        help += "-h, --help                                                     : ヘルプの表示";
        println(help.mkString("\n"));
      }
    }
  }

  def debug(): Unit = {
    var prefixtree: PrefixTree[Morpheme] = null;
    var matrix: Matrix = null;
    def go(): Unit = {
      print("command : ");
      readLine.split(" ").toList match {
        case "exit" :: xs => return ();
        case "deserialize" :: dict :: xs => {
          val dictSet = DictionarySet(dict);
          prefixtree = dictSet.prefixtree;
          matrix = dictSet.matrix;
        }
        case "cost" :: l :: r :: xs => {
          val left  = Util.toIntOption(l);
          val right = Util.toIntOption(r);
          if (matrix != null && left != None && right != None) {
            println(matrix.getCost(left.get, right.get));
          } else {
            println("matrix is null or invalid args");
          }
        }
        case "find" :: surface :: xs => {
          if (prefixtree != null) {
            prefixtree.find(surface) match {
              case None    => println("not found");
              case Some(m) => println(m);
            }
          } else {
            println("prefixtree is null");
          }
        }
        case "search" :: surface :: xs => {
          if (prefixtree != null) {
            prefixtree.prefixSearch(surface).foreach(println(_));
          } else {
            println("prefixtree is null");
          }
        }
        case "status" :: xs => {
          if (prefixtree != null && matrix != null) {
            println("----- matrix -----");
            println("leftSize=%d, rightSize=%d".format(matrix.leftSize, matrix.rightSize));
            println("----- prefixtree -----");
            println("size=%d".format(prefixtree.size));
          } else {
            println("dict is null");
          }
        }
        case _ => {
          val help = ListBuffer[String]();
          help += "exit               : デバッグモードを終了する";
          help += "deserialize [DICT] : 構築済み辞書を読み込む";
          println(help.mkString("\n"));
        }
      }
      go();
    }
    go();
  }
}
