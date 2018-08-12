package com.github.ng3rdstmadgke.midomoji;

import scala.io.Source;
import scala.io.StdIn.readLine;
import scala.collection.mutable.ListBuffer;
import java.text.Normalizer;
import java.io.{FileOutputStream, FileInputStream};

object Main {
  def main(args: Array[String]): Unit = {
    args.toList match {
      // --build-dict ./dictionary/morpheme.csv ./dictionary/dict.bin
      case ("--build-dict") :: morphemePath :: dictBin :: xs => {
        val parse = (arr: Array[String]) => {
          val Array(surface, left, right, cost, pos, k1, k2, base, yomi, pron) = arr;
          Array(left, right, cost, pos, k1, k2).map(_.toInt);
        }
        val add = (existing: List[Array[Int]], elem: Array[Int]) => {
          existing match {
            case ls: List[Array[Int]] => elem :: ls;
            case _                    => List[Array[Int]](elem);
          }
        }
        val prefixtree = PrefixTree.build[Array[Int]](morphemePath)(parse)(add);
        PrefixTreeSerializeObject.serialize[Array[Array[Int]]](prefixtree, dictBin);
      }

      // --build-matrix ./dictionary/matrix.def ./dictionary/matrix.bin
      case ("--build-matrix") :: matrixPath :: matrixBin :: xs => {
        val matrix = Matrix.build(matrixPath);
        Util.kryoSerialize[Matrix](matrix, matrixBin);
      }

      // --build-config ./dictionary/char.tsv ./dictionary/char_type.tsv ./dictionary/unk.tsv ./dictionary/config.bin
      case ("--build-config") :: charPath :: charTypePath :: unkPath :: configBin :: xs => {
        val charType = CharType.buildCharType(charPath, charTypePath, unkPath);
        Util.kryoSerialize[CharType](charType, configBin);
      }

      // --build-pos-config ./dictionary/pos.tsv ./dictionary/katsuyou_gata.tsv ./dictionary/katsuyou_kei.tsv ./dictionary/pos_config.bin
      case ("--build-pos-config") :: posPath :: katsuyouGataPath :: katsuyouKeiPath :: posConfigBin :: xs => {
        val posConfig = PosConfig.build(posPath, katsuyouGataPath, katsuyouKeiPath);
        Util.kryoSerialize[PosConfig](posConfig, posConfigBin);
      }

      // --check-dict ./dictionary/morpheme.csv ./dictionary/dict.bin
      case ("--check-dict") :: morphemePath :: dictBin :: xs => {
        var prefixtree = PrefixTreeSerializeObject.deserialize[Array[Array[Int]]](dictBin);
        val parse = (arr: Array[String]) => {
          val Array(surface, left, right, cost, pos, k1, k2, base, yomi, pron) = arr;
          Array(left, right, cost, pos, k1, k2).map(_.toInt);
        }
        val exists = (elem: Array[Int], es: Array[Array[Int]]) => es.exists(e => elem.sameElements(e));
        PrefixTree.check[Array[Int]](prefixtree, morphemePath)(parse)(exists);
      }

      // --check-matrix ./dictionary/matrix.def ./dictionary/matrix.bin
      case ("--check-matrix") :: matrixPath :: matrixBin :: xs => {
        val matrix = Util.kryoDeserialize[Matrix](matrixBin);
        Matrix.check(matrix, matrixPath);
      }

      // --debug ./dictionary/dict.bin ./dictionary/matrix.bin ./dictionary/config.bin ./dictionary/pos_config.bin
      case ("--debug") :: dictBin :: matrixBin :: configBin :: posConfigBin :: xs => {
        debug(dictBin, matrixBin, configBin, posConfigBin);
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

  def debug(dictBin: String, matrixBin: String, configBin: String, posConfigBin: String): Unit = {
    var prefixtree = PrefixTreeSerializeObject.deserialize[Array[Array[Int]]](dictBin);
    var matrix     = Util.kryoDeserialize[Matrix](matrixBin);
    var charType   = Util.kryoDeserialize[CharType](configBin);
    var posConfig  = Util.kryoDeserialize[PosConfig](posConfigBin);
    def go(): Unit = {
      print("command : ");
      readLine.split(" ").toList match {
        case "init" :: xs => {
          prefixtree = PrefixTree[Array[Array[Int]]](5);
          matrix     = Matrix(1316, 1316);
          charType   = new CharType(new Array[Array[Int]](0), new Array[TokenConfig](0));
          posConfig  = new PosConfig(Array[String](), Array[String](), Array[String]());
        }
        case "serialize" :: xs => {
          PrefixTreeSerializeObject.serialize[Array[Array[Int]]](prefixtree, dictBin);
          Util.kryoSerialize[Matrix](matrix, matrixBin);
          Util.kryoSerialize[CharType](charType, configBin);
          Util.kryoSerialize[PosConfig](posConfig, posConfigBin);
        }
        case "deserialize" :: xs => {
          prefixtree = PrefixTreeSerializeObject.deserialize[Array[Array[Int]]](dictBin);
          matrix     = Util.kryoDeserialize[Matrix](matrixBin);
          charType   = Util.kryoDeserialize[CharType](configBin);
          posConfig  = Util.kryoDeserialize[PosConfig](posConfigBin);
        }
        case "cost" :: l :: r :: xs => {
          try {
            println(matrix.getCost(l.toInt, r.toInt));
          } catch {
            case _ : Throwable => println("invalid args");;
          }
        }
        case "find" :: text :: xs => {
          prefixtree.find(text) match {
            case None    => println("not found");
            case Some(m) => println(m);
          }
        }
        case "search" :: text :: xs => {
          val len = text.length;
          (0 until len).foreach { i =>
            val sub = text.slice(i, len);
            println("%d :".format(i + 1));
            prefixtree.prefixSearch(sub).foreach { e =>
              val surface = e._1;
              val data = e._2.map(d => "(" + d.mkString(", ") + ")").mkString(", ");
              println("  %s : %s".format(surface, data));
            }
          }
        }
        case "tokenize" :: text :: xs => {
          val len = text.length;
          val tokenizer = new Tokenizer[Array[Array[Int]]](charType, prefixtree);
          val lattice = tokenizer.tokenize(text, Array.fill[List[LatticeNode]](len + 2)(Nil));
          println("0 : BOS");
          (1 to len).foreach { i =>
            println("%d : ".format(i));
            println(lattice(i).map("  " + _.toDebugString(posConfig)).mkString("\n"));
          }
          println("%d : EOS".format(len + 1));
        }
        case "analyze" :: text :: xs => {
          val normalized = Normalizer.normalize(text, Normalizer.Form.NFKC);
          val viterbi = Viterbi(prefixtree, matrix, charType);
          viterbi.analize(normalized) match {
            case None => println("ノードが途中で途切れました");
            case Some((list, cost)) => {
              println("cost : " + cost);
              println(list.reverse.map(_.toDebugString(posConfig)).mkString("\n"));
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
          help += "init                                 : 辞書をリセットする";
          help += "serialize <DICT> <MATRIX> <CONFIG>   : 構築済み辞書を読み込む";
          help += "deserialize <DICT> <MATRIX> <CONFIG> : 構築済み辞書を読み込む";
          help += "cost <LEFT> <RIGHT>                  : 連接コストを表示する";
          help += "find <TEXT>                          : トライ木に対してSURFACEをキーとする値を取り出す";
          help += "search <TEXT>                        : トライ木に対して共通接頭辞検索を行う";
          help += "tokenize <TEXT>                      : 未知語ノードの生成を行う";
          help += "analyze <TEXT>                       : 形態素解析を行う";
          help += "add <SURFACE>                        : トライ木に要素を追加する";
          help += "dump                                 : トライ木をダンプする";
          help += "status                               : 辞書のステータスを表示する";
          help += "exit                                 : デバッグモードを終了する";
          println(help.mkString("\n"));
        }
      }
      go();
    }
    go();
  }
}
