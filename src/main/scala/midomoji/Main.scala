package com.github.ng3rdstmadgke.midomoji;

import scala.io.Source;
import scala.io.StdIn.readLine;
import scala.collection.mutable.ListBuffer;
import java.text.Normalizer;
import java.io.{FileOutputStream, FileInputStream};

object Main {
  def main(args: Array[String]): Unit = {
    args.toList match {
      // --build-dict ./dictionary/morpheme.tsv ./dictionary/dict.bin
      case ("--build-dict") :: morphemePath :: dictBin :: Nil => {
        val parse = (arr: Array[String], id: Int) => {
          val Array(surface, left, right, cost, pos, base, yomi, pron) = arr;
          // leftId, rightId, genCost, posId, id, beginIdx, endIdx, totalCost, pointer
          Array(left.toInt, right.toInt, cost.toInt, pos.toInt, id, -1, -1, -1, -1);
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
      case ("--build-matrix") :: matrixPath :: matrixBin :: Nil => {
        val matrix = Matrix.build(matrixPath);
        Util.kryoSerialize[Matrix](matrix, matrixBin);
      }

      // --build-config ./dictionary/char.tsv ./dictionary/char_type.tsv ./dictionary/unk.tsv ./dictionary/config.bin
      case ("--build-config") :: charPath :: charTypePath :: unkPath :: configBin :: Nil => {
        val charType = CharType.build(charPath, charTypePath, unkPath);
        Util.kryoSerialize[CharType](charType, configBin);
      }

      // --build-pos-info ./dictionary/pos.tsv ./dictionary/pos_info.bin
      case ("--build-pos-info") :: posPath :: posInfoBin :: Nil => {
        val posInfo = PosInfo.build(posPath);
        Util.kryoSerialize[PosInfo](posInfo, posInfoBin);
      }

      // --check-dict ./dictionary/morpheme.tsv ./dictionary/dict.bin
      case ("--check-dict") :: morphemePath :: dictBin :: Nil => {
        var prefixtree = PrefixTreeSerializeObject.deserialize[Array[Array[Int]]](dictBin);
        val parse = (arr: Array[String], id: Int) => {
          val Array(surface, left, right, cost, pos, base, yomi, pron) = arr;
          Array(left.toInt, right.toInt, cost.toInt, pos.toInt, id, -1, -1, -1, -1);
        }
        val exists = (elem: Array[Int], es: Array[Array[Int]]) => es.exists(e => elem.sameElements(e));
        PrefixTree.check[Array[Int]](prefixtree, morphemePath)(parse)(exists);
      }

      // --check-matrix ./dictionary/matrix.def ./dictionary/matrix.bin
      case ("--check-matrix") :: matrixPath :: matrixBin :: Nil => {
        val matrix = Util.kryoDeserialize[Matrix](matrixBin);
        Matrix.check(matrix, matrixPath);
      }

      // --debug ./dictionary/dict.bin ./dictionary/matrix.bin ./dictionary/config.bin ./dictionary/pos_info.bin
      case ("--debug") :: dictBin :: matrixBin :: configBin :: posInfoBin :: Nil => {
        debug(dictBin, matrixBin, configBin, posInfoBin);
      }

      // --analyze ./dictionary/dict.bin ./dictionary/matrix.bin ./dictionary/config.bin ./dictionary/pos_info.bin
      case ("--analyze") :: dictBin :: matrixBin :: configBin :: posInfoBin :: Nil => {
        analyze(dictBin, matrixBin, configBin, posInfoBin);
      }

      // --analyze ./dictionary/dict.bin ./dictionary/matrix.bin ./dictionary/config.bin ./dictionary/pos_info.bin
      case ("--analyze") :: dictBin :: matrixBin :: configBin :: posInfoBin :: targetPath :: Nil => {
        analyze(dictBin, matrixBin, configBin, posInfoBin, targetPath);
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

  def analyze(dictBin: String, matrixBin: String, configBin: String, posInfoBin: String, targetPath: String = null): Unit = {
    var prefixtree = PrefixTreeSerializeObject.deserialize[Array[Array[Int]]](dictBin);
    var matrix     = Util.kryoDeserialize[Matrix](matrixBin);
    var charType   = Util.kryoDeserialize[CharType](configBin);
    var posInfo  = Util.kryoDeserialize[PosInfo](posInfoBin);
    def _analyze(text: String): String = {
      val normalized = Normalizer.normalize(text, Normalizer.Form.NFKC);
      val viterbi = Viterbi(prefixtree, matrix, charType);
      viterbi.analyze(normalized) match {
        case Some(node) => node.mkString("\n");
        case None => {
          System.err.println("ノードが途中で途切れました");
          "";
        }
      }
    }
    if (targetPath == null) {
      var line = readLine;
      while (line != null) {
        _analyze(line);
        line = readLine;
      }
    } else {
      Using[Source, Unit](Source.fromFile(targetPath)) { s =>
        val sb = new StringBuilder(300000);
        s.getLines.foreach { line =>
          if (sb.length() > 300000) {
            println(sb);
            sb.setLength(0);
          }
          sb.append(_analyze(line));
        }
        println(sb);
      }
    }
  }

  def debug(dictBin: String, matrixBin: String, configBin: String, posInfoBin: String): Unit = {
    var prefixtree = PrefixTreeSerializeObject.deserialize[Array[Array[Int]]](dictBin);
    var matrix     = Util.kryoDeserialize[Matrix](matrixBin);
    var charType   = Util.kryoDeserialize[CharType](configBin);
    var posInfo  = Util.kryoDeserialize[PosInfo](posInfoBin);
    def go(): Unit = {
      print("command : ");
      readLine.split(" ").toList match {
        case "init" :: xs => {
          prefixtree = PrefixTree[Array[Array[Int]]](5);
          matrix     = Matrix(1316, 1316);
          charType   = new CharType(new Array[Array[Int]](0), new Array[TokenConfig](0));
          posInfo  = new PosInfo(Array[String]());
        }
        case "serialize" :: xs => {
          prefixtree.optimize;
          PrefixTreeSerializeObject.serialize[Array[Array[Int]]](prefixtree, dictBin);
          Util.kryoSerialize[Matrix](matrix, matrixBin);
          Util.kryoSerialize[CharType](charType, configBin);
          Util.kryoSerialize[PosInfo](posInfo, posInfoBin);
        }
        case "deserialize" :: xs => {
          prefixtree = PrefixTreeSerializeObject.deserialize[Array[Array[Int]]](dictBin);
          matrix     = Util.kryoDeserialize[Matrix](matrixBin);
          charType   = Util.kryoDeserialize[CharType](configBin);
          posInfo  = Util.kryoDeserialize[PosInfo](posInfoBin);
        }
        case "cost" :: l :: r :: xs => {
          try {
            println(matrix.getCost(l.toInt, r.toInt));
          } catch {
            case _ : Throwable => println("invalid args");;
          }
        }
        case "find" :: text :: xs => {
          def dataToString: Array[Array[Int]] => String = arr => arr.map(e => "(" + e(0) + ", " + e(1) + ", " + e(2) + ")").mkString(", ");
          prefixtree.debugFind(text)(dataToString);
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
            lattice(i).foreach(println(_));
          }
          println("%d : EOS".format(len + 1));
        }
        case "analyze" :: text :: xs => {
          val normalized = Normalizer.normalize(text, Normalizer.Form.NFKC);
          val viterbi = Viterbi(prefixtree, matrix, charType);
          viterbi.analyze(normalized) match {
            case None => System.err.println("ノードが途中で途切れました");
            case Some(node) => {
              println(node.mkString("\n"));
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
          println(xs);
          xs match {
            case s :: e :: rest => prefixtree.dump(s.toInt, e.toInt);
            case _              => prefixtree.dump();
          }
        }
        case "status" :: xs => {
          prefixtree.status;
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
