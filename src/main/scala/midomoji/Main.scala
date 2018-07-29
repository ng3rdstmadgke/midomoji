package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;
import scala.io.StdIn.readLine;
import scala.collection.mutable.{ListBuffer, HashMap};
import java.nio.file.{Paths, Files};

object Main {

  
  def main(args: Array[String]): Unit = {
    args.toList match {
      // -b ./dictionary/matrix.def ./dictionary/morpheme.csv ./dictionary/dictionary_set.bin
      case ("-b" | "--build") :: mtPath :: ptPath :: dictPath :: xs => {
        val matrix = Util.createMT(mtPath);
        val prefixtree = Util.createPT(ptPath);
        DictionarySet(prefixtree, matrix).serialize(dictPath);
      }

      // -cm ./dictionary/dictionary_set.bin ./dictionary/matrix.def
      case ("-cm" | "--check-matrix") :: dictPath :: mtPath :: xs => {
        val dictSet = DictionarySet(dictPath);
        val mt = dictSet.matrix;
        Util.checkMT(mt, mtPath);
      }

      // -cp ./dictionary/dictionary_set.bin ./dictionary/morpheme.csv
      case ("-cp" | "--check-prefixtree") :: dictPath :: ptPath :: xs => {
        val dictSet = DictionarySet(dictPath);
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
    var prefixtree: PrefixTree[Morpheme] = PrefixTree[Morpheme](500000);
    var matrix: Matrix = Matrix(1316, 1316);
    def go(): Unit = {
      print("command : ");
      readLine.split(" ").toList match {
        case "exit" :: xs => return ();
        case "deserialize" :: dict :: xs => {
          val dictSet = DictionarySet(dict);
          prefixtree = dictSet.prefixtree;
          matrix = dictSet.matrix;
        }
        case "create" :: dict :: xs => {
          prefixtree = Util.createPT(dict);
        }
        case "check" :: ptPath :: xs => {
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
            case Some(m) => println(m);
          }
        }
        case "search" :: surface :: xs => {
          prefixtree.prefixSearch(surface).foreach(println(_));
        }
        case "add" :: surface :: xs => {
          val m = Morpheme(surface, 1, 1, 0, "", "", "");
          prefixtree.add(surface, m);
        }
        case "init" :: xs => {
          prefixtree = PrefixTree[Morpheme](500000);
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
