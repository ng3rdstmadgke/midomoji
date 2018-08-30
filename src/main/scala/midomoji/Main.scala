package com.github.ng3rdstmadgke.midomoji;

import scala.io.Source;
import scala.io.StdIn.readLine;
import scala.collection.mutable.{ListBuffer, HashMap};
import java.text.Normalizer;
import java.io.{BufferedWriter, BufferedReader, OutputStreamWriter, InputStreamReader, OutputStream, InputStream, FileOutputStream, FileInputStream};

object Main {
  def parse(arr: Array[String], id: Int): Array[Int] = {
    val Array(surface, left, right, cost, pos, base, yomi, pron) = arr;
    Array(left.toInt, right.toInt, cost.toInt, pos.toInt, id);
  }

  def add(existing: List[Array[Int]], elem: Array[Int]): List[Array[Int]] = {
    existing match {
      case null => List[Array[Int]](elem);
      case Nil  => List[Array[Int]](elem);
      case list => elem :: list;
    }
  }


  def main(args: Array[String]): Unit = {
    OptionParser.parse(args) match {
      case ("build-dict", argMap) if argMap.contains("dict-dir") => {
        val dictDir = argMap("dict-dir");
        val prefixtree = PrefixTree.build[Array[Int]](Util.morphemeTsv(dictDir))(parse)(add);
        PrefixTreeSerializeObject.serialize[Array[Array[Int]]](prefixtree, Util.dictBin(dictDir));
      }
      case ("build-matrix", argMap) if argMap.contains("dict-dir") => {
        val dictDir = argMap("dict-dir");
        val matrix = Matrix.build(Util.matrixTsv(dictDir));
        Util.kryoSerialize[Matrix](matrix, Util.matrixBin(dictDir));
      }
      case ("build-config", argMap) if argMap.contains("dict-dir") => {
        val dictDir = argMap("dict-dir");
        val charType = CharType.build(Util.charTsv(dictDir), Util.charTypeTsv(dictDir), Util.unkTsv(dictDir));
        Util.kryoSerialize[CharType](charType, Util.configBin(dictDir));
      }
      case ("build-pos-info", argMap) if argMap.contains("dict-dir") => {
        val dictDir = argMap("dict-dir");
        val posInfo = PosInfo.build(Util.posTsv(dictDir));
        Util.kryoSerialize[PosInfo](posInfo, Util.posInfoBin(dictDir));
      }
      case ("build-meta-info", argMap) if argMap.contains("dict-dir") => {
        val dictDir = argMap("dict-dir");
        val metaInfo = MetaInfo.build(Util.morphemeTsv(dictDir));
        Util.kryoSerialize[MetaInfo](metaInfo, Util.metaInfoBin(dictDir));
      }
      case ("check-dict", argMap) if argMap.contains("dict-dir") => {
        val dictDir = argMap("dict-dir");
        var prefixtree = PrefixTreeSerializeObject.deserialize[Array[Array[Int]]](Util.dictBin(dictDir));
        val exists = (elem: Array[Int], es: Array[Array[Int]]) => es.exists(e => elem.sameElements(e));
        val start = System.currentTimeMillis;
        PrefixTree.check[Array[Int]](prefixtree, Util.morphemeTsv(dictDir))(parse)(exists);
        val end = System.currentTimeMillis;
        println("time(ms) : " + (end - start));
      }
      case ("check-matrix", argMap) if argMap.contains("dict-dir") => {
        val dictDir = argMap("dict-dir");
        val matrix = Util.kryoDeserialize[Matrix](Util.matrixBin(dictDir));
        val start = System.currentTimeMillis;
        Matrix.check(matrix, Util.matrixTsv(dictDir));
        val end = System.currentTimeMillis;
        println("time(ms) : " + (end - start));
      }
      case ("analyze", argMap) => {
        val prefixtree = PrefixTreeSerializeObject.deserializeFromResource[Array[Array[Int]]](Util.dictBin());
        val matrix     = Util.kryoDeserializeFromResource[Matrix](Util.matrixBin());
        val charType   = Util.kryoDeserializeFromResource[CharType](Util.configBin());
        val midomoji = new Midomoji(prefixtree, matrix, charType);
        val format = if (argMap.contains("format")) argMap("format") else "simple";
        val is = if (argMap.contains("input"))  new FileInputStream(argMap("input"))   else System.in;
        val os = if (argMap.contains("output")) new FileOutputStream(argMap("output")) else System.out;
        val bs = if (argMap.contains("buffer-size")) argMap("buffer-size").toInt else 8192;
        midomoji.analyzeInput(is, os, bs)(Midomoji.format(format));
      }
      case ("debug", argMap) => {
        debug();
      }
      case _ => {
        OptionParser.help();
      }
    }
  }

  def debug(): Unit = {
    var prefixtree = PrefixTree[Array[Array[Int]]](5);
    var matrix     = Matrix(1316, 1316);
    var charType   = new CharType(new Array[Array[Int]](0), new Array[TokenConfig](0), HashMap[(Int, Int), Array[Int]]());
    var posInfo    = new PosInfo(Array[String]());
    var metaInfo   = new MetaInfo(Array[Array[String]]());
    def go(): Unit = {
      print("command : ");
      readLine.split(" ").map(_.trim).toList match {
        case "init" :: xs => {
          prefixtree = PrefixTree[Array[Array[Int]]](5);
          matrix     = Matrix(1316, 1316);
          charType   = new CharType(new Array[Array[Int]](0), new Array[TokenConfig](0), HashMap[(Int, Int), Array[Int]]());
          posInfo    = new PosInfo(Array[String]());
          metaInfo   = new MetaInfo(Array[Array[String]]());
        }
        case "load" :: xs => {
          if (xs.isEmpty) {
            prefixtree = PrefixTreeSerializeObject.deserializeFromResource[Array[Array[Int]]](Util.dictBin());
            matrix     = Util.kryoDeserializeFromResource[Matrix](Util.matrixBin());
            charType   = Util.kryoDeserializeFromResource[CharType](Util.configBin());
            posInfo    = Util.kryoDeserializeFromResource[PosInfo](Util.posInfoBin());
            metaInfo   = Util.kryoDeserializeFromResource[MetaInfo](Util.metaInfoBin());
          } else {
            val dictDir = xs.head;
            prefixtree = PrefixTreeSerializeObject.deserialize[Array[Array[Int]]](Util.dictBin(dictDir));
            matrix     = Util.kryoDeserialize[Matrix](Util.matrixBin(dictDir));
            charType   = Util.kryoDeserialize[CharType](Util.configBin(dictDir));
            posInfo    = Util.kryoDeserialize[PosInfo](Util.posInfoBin(dictDir));
            metaInfo   = Util.kryoDeserialize[MetaInfo](Util.metaInfoBin(dictDir));
          }
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
            println("%d : %s".format(i + 1, sub));
            prefixtree.prefixSearch(sub).foreach { e =>
              val surface = e._1;
              val data = e._2.map(d => "(" + d.mkString(", ") + ")").mkString(", ");
              println("  %s : %s".format(surface, data));
            }
          }
        }
        case "tokenize" :: text :: xs => {
          val lattice = new Lattice(prefixtree, charType, matrix);
          lattice.build(text);
          lattice.getLattice().zipWithIndex.foreach { elem =>
            val (nodes, idx) = elem;
            println("[" + idx + "] : ");
            val str = nodes.map { n =>
              val Array(startIdx, endIdx, leftId, rightId, genCost, posId, id, totalCost, nextNodeIdx) = n;
              val unk = if (id == -1) " (未)" else "";
              "  (" + n.mkString(",") + ") : " + text.slice(startIdx, endIdx) + unk;
            }.mkString("\n");
            println(str);
          }
        }
        case "analyze" :: text :: xs => {
          val lattice = new Lattice(prefixtree, charType, matrix);
          val str = lattice.analyze(text).map { n =>
            val Array(startIdx, endIdx, leftId, rightId, genCost, posId, id, totalCost, nextNodeIdx) = n;
            val surface = text.slice(startIdx, endIdx);
            val pos = posInfo.getPos(posId);
            val base = metaInfo.getBaseForm(id, surface);
            val yomi = metaInfo.getYomi(id, surface);
            "%s\t%d\t%d\t%d\t%s\t%s\t%s".format(surface, leftId, rightId, genCost, pos, base, yomi);
          }
          println("BOS\n" + str.mkString("\n") + "\nEOS\n");
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
          help += "init                : 辞書をリセットする";
          help += "load [DICT_DIR]     : 辞書を読み込む。DICT_DIRを指定しない場合はresourcesから読み込む";
          help += "cost <LEFT> <RIGHT> : 連接コストを表示する";
          help += "find <TEXT>         : トライ木に対してSURFACEをキーとする値を取り出す";
          help += "search <TEXT>       : トライ木に対して共通接頭辞検索を行う";
          help += "tokenize <TEXT>     : 未知語ノードの生成を行う";
          help += "analyze <TEXT>      : 形態素解析を行う";
          help += "add <SURFACE>       : トライ木に要素を追加する";
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
