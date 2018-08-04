package com.github.ng3rdstmadgke.midomoji;

import scala.io.Source;
import scala.io.StdIn.readLine;
import scala.collection.mutable.{ListBuffer, HashMap};
import java.text.Normalizer;

object Main {

  
  def main(args: Array[String]): Unit = {
    args.toList match {
      // --build ./dictionary/matrix.def ./dictionary/pos-id.def ./dictionary/morpheme.csv ./dictionary/dictionary_set.bin
      case ("--build") :: mtPath :: posPath :: ptPath :: dictPath :: xs => {
        val matrix = Util.createMT(mtPath);
        val (posMap, posArr) = Util.createPosMap(posPath);
        val prefixtree = Util.createPT(ptPath, posMap);
        DictionarySet[Array[Array[Int]]](prefixtree, matrix, posArr).serialize(dictPath);
      }

      // --check-matrix ./dictionary/dictionary_set.bin ./dictionary/matrix.def
      case ("--check-matrix") :: dictPath :: mtPath :: xs => {
        val dictSet = DictionarySet[Array[Array[Int]]](dictPath);
        val mt = dictSet.matrix;
        Util.checkMT(mt, mtPath);
      }

      // --check-prefixtree ./dictionary/dictionary_set.bin ./dictionary/morpheme.csv
      case ("--check-prefixtree") :: dictPath :: ptPath :: xs => {
        val dictSet = DictionarySet[Array[Array[Int]]](dictPath);
        val pt = dictSet.prefixtree;
        Util.checkPT(pt, ptPath);
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
        help += "-d, --debug                                       : デバッグモード起動";
        help += "-h, --help                                        : ヘルプの表示";
        println(help.mkString("\n"));
      }
    }
  }

  def debug(): Unit = {
    var prefixtree = PrefixTree[Array[Array[Int]]](5);
    var matrix     = Matrix(1316, 1316);
    var posArr     = Array[Array[String]]();
    def go(): Unit = {
      print("command : ");
      readLine.split(" ").toList match {
        case "init" :: xs => {
          prefixtree = PrefixTree[Array[Array[Int]]](5);
          matrix     = Matrix(1316, 1316);
          posArr     = Array[Array[String]]();
        }
        case "deserialize" :: dict :: xs => {
          val dictSet = DictionarySet[Array[Array[Int]]](dict);
          prefixtree = dictSet.prefixtree;
          matrix     = dictSet.matrix;
          posArr     = dictSet.posArr;
        }
        case "pos" :: xs => {
          posArr.zipWithIndex.foreach { e =>
            val idx = e._2;
            val pos = e._1.mkString(", ");
            println(idx + " : " + pos);
          }
        }
        case "cost" :: l :: r :: xs => {
          try {
            println(matrix.getCost(l.toInt, r.toInt));
          } catch {
            case _ : Throwable => println("invalid args");;
          }
        }
        case "find" :: surface :: xs => {
          prefixtree.find(surface) match {
            case None    => println("not found");
            case Some(m) => println(m);
          }
        }
        case "search" :: surface :: xs => {
          val len = surface.length;
          (0 until len).foreach { i =>
            val sub = surface.slice(i, len);
            println("%d :".format(i + 1));
            prefixtree.prefixSearch(sub).foreach { e =>
              val surface = e._1;
              val data = e._2.map(d => "(" + d.mkString(", ") + ")").mkString(", ");
              println("  %s : %s".format(surface, data));
            }
          }
        }
        case "analyze" :: text :: xs => {
          val normalized = Normalizer.normalize(text, Normalizer.Form.NFKC);
          val viterbi = Viterbi(prefixtree, matrix);
          viterbi.analize(normalized) match {
            case None => println("ノードが途中で途切れました");
            case Some((list, cost)) => {
              println("cost : " + cost);
              println(list.tail.reverse.mkString("\n"));
            }
          }
        }
        case "add" :: surface :: xs => {
          prefixtree.add(surface, Array(1,1,1,1)) { (existing, elem) =>
            existing match {
              case arr: Array[Array[Int]] => {
                val len = arr.length;
                val newArr = new Array[Array[Int]](len + 1);
                (0 until len).foreach(i => newArr(i) = arr(i));
                newArr(len) = elem;
                newArr;
              }
              case _ => Array[Array[Int]](elem);
            }
          };
        }
        case "dump" :: xs => {
          prefixtree.dump;
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
        case "exit" :: xs => return ();
        case _ => {
          val help = ListBuffer[String]();
          help += "deserialize [DICT]  : 構築済み辞書を読み込む";
          help += "pos                 : 品詞リストをすべて表示する";
          help += "cost [LEFT] [RIGHT] : 連接コストを表示する";
          help += "find [SURFACE]      : トライ木に対してSURFACEをキーとする値を取り出す";
          help += "search [TEXT]       : トライ木に対して共通接頭辞検索を行う";
          help += "analyze [TEXT]      : 形態素解析を行う";
          help += "add [SURFACE]       : トライ木に要素を追加する";
          help += "init                : 辞書をリセットする";
          help += "dump                : トライ木をダンプする";
          help += "status              : 辞書のステータスを表示する";
          help += "exit                : デバッグモードを終了する";
          println(help.mkString("\n"));
        }
      }
      go();
    }
    go();
  }
}
