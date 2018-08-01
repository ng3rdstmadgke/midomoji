package com.github.ng3rdstmadgke.midomoji;

import scala.io.Source;
import scala.io.StdIn.readLine;
import scala.collection.mutable.{ListBuffer, HashMap};
import java.text.Normalizer;

object Main {

  
  def main(args: Array[String]): Unit = {
    args.toList match {
      // --build ./dictionary/matrix.def ./dictionary/morpheme.csv ./dictionary/dictionary_set.bin
      case ("--build") :: mtPath :: ptPath :: dictPath :: xs => {
        val matrix = Util.createMT(mtPath);
        val prefixtree = Util.createPT(ptPath);
        DictionarySet[Array[Int]](prefixtree, matrix).serialize(dictPath);
      }

      // --check-matrix ./dictionary/dictionary_set.bin ./dictionary/matrix.def
      case ("-cm" | "--check-matrix") :: dictPath :: mtPath :: xs => {
        val dictSet = DictionarySet[Array[Int]](dictPath);
        val mt = dictSet.matrix;
        Util.checkMT(mt, mtPath);
      }

      // --check-prefixtree ./dictionary/dictionary_set.bin ./dictionary/morpheme.csv
      case ("--check-prefixtree") :: dictPath :: ptPath :: xs => {
        val dictSet = DictionarySet[Array[Int]](dictPath);
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
    var prefixtree: PrefixTree[Array[Int]] = PrefixTree[Array[Int]](500000);
    var matrix: Matrix = Matrix(1316, 1316);
    def go(): Unit = {
      print("command : ");
      readLine.split(" ").toList match {
        case "exit" :: xs => return ();
        case "deserialize" :: dict :: xs => {
          val dictSet = DictionarySet[Array[Int]](dict);
          prefixtree = dictSet.prefixtree;
          matrix = dictSet.matrix;
        }
        case "create" :: dict :: xs =>{
          prefixtree = Util.createPT(dict);
        }
        case "check" :: ptPath :: xs =>{
          Util.checkPT(prefixtree, ptPath);
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
          prefixtree.find(surface) match {
            case None    => println("not found");
            case Some(m) => println(m.map(e => "(" + e.mkString(", ") + ")"));
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
        case "analize" :: text :: xs => {
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
          prefixtree.add(surface, Array(1,1,1));
        }
        case "init" :: xs => {
          prefixtree = PrefixTree[Array[Int]](5);
          matrix = Matrix(1316, 1316);
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
        case _ => {
          val help = ListBuffer[String]();
          help += "exit                : デバッグモードを終了する";
          help += "deserialize [DICT]  : 構築済み辞書を読み込む";
          help += "create [DICT]       : ";
          help += "check [DICT] [PT]   : ";
          help += "cost [LEFT] [RIGHT] : ";
          help += "find [SURFACE]      : ";
          help += "search [TEXT]       : ";
          help += "add [SURFACE]       : ";
          help += "init                : ";
          help += "dump                : ";
          help += "status              : ";
          println(help.mkString("\n"));
        }
      }
      go();
    }
    go();
  }
}
