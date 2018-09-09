package com.github.ng3rdstmadgke.midomoji;

import scala.io.Source;
import scala.io.StdIn.readLine;
import scala.collection.mutable.{ListBuffer, HashMap};
import java.text.Normalizer;
import java.io.{BufferedWriter, BufferedReader, OutputStreamWriter, InputStreamReader, OutputStream, InputStream, FileOutputStream, FileInputStream};

object Main {

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
        val t1 = System.currentTimeMillis;
        val legacyPrefixtree = LegacyPrefixTree.build(Util.morphemeTsv(dictDir));
        val t2 = System.currentTimeMillis;
        val prefixtree = legacyPrefixtree.toDoubleArray();
        val t3 = System.currentTimeMillis;
        printTime("build legacy prefixtree", t2 - t1, 25);
        printTime("build prefixtree", t3 - t2, 25);
        PrefixTreeSerializeObject.serialize[Array[Long]](prefixtree, Util.dictBin(dictDir));
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
        var prefixtree = PrefixTreeSerializeObject.deserialize[Array[Long]](Util.dictBin(dictDir));
        val start = System.currentTimeMillis;
        PrefixTree.check(prefixtree, Util.morphemeTsv(dictDir));
        val end = System.currentTimeMillis;
        printTime("check dict", end - start);
      }
      case ("check-matrix", argMap) if argMap.contains("dict-dir") => {
        val dictDir = argMap("dict-dir");
        val matrix = Util.kryoDeserialize[Matrix](Util.matrixBin(dictDir));
        val start = System.currentTimeMillis;
        Matrix.check(matrix, Util.matrixTsv(dictDir));
        val end = System.currentTimeMillis;
        printTime("check matrix", end - start);
      }
      case ("check-user-dict", argMap) if argMap.contains("dict-dir") => {
        val dictDir = argMap("dict-dir");
        val path = Util.morphemeTsv(dictDir);
        val t1 = System.currentTimeMillis;
        val userDict = LegacyPrefixTree.build(path);
        val t2 = System.currentTimeMillis;
        LegacyPrefixTree.check(path, userDict);
        val t3 = System.currentTimeMillis;
        printTime("build dictionary", t2 - t1);
        printTime("check dictionary", t3 - t2);
      }
      case ("analyze", argMap) => {
        val t1 = System.currentTimeMillis();
        val prefixtree = PrefixTreeSerializeObject.deserializeFromResource[Array[Long]](Util.dictBin());
        val matrix     = Util.kryoDeserializeFromResource[Matrix](Util.matrixBin());
        val charType   = Util.kryoDeserializeFromResource[CharType](Util.configBin());
        val format = if (argMap.contains("format")) argMap("format") else "simple";
        val is = if (argMap.contains("input"))  new FileInputStream(argMap("input"))   else System.in;
        val os = if (argMap.contains("output")) new FileOutputStream(argMap("output")) else System.out;
        val bs = if (argMap.contains("buffer-size")) argMap("buffer-size").toInt else 8192;
        // ユーザー辞書の構築
        // ファイルの形式は「SURFACE	LEFT_ID	RIGHT_ID	GEN_COST」
        val userDict = argMap.get("user-dict") match {
          case None       => new LegacyPrefixTree[Long]();
          case Some(path) => LegacyPrefixTree.build(path);
        }
        val t2 = System.currentTimeMillis();
        val midomoji = new Midomoji(prefixtree, matrix, charType, userDict);
        midomoji.analyzeInput(is, os, bs)(Midomoji.format(format));
        val t3 = System.currentTimeMillis();
        if (argMap.contains("debug")) {
          printTime("load", t2 - t1);
          printTime("analyze", t3 - t2);
        }
      }
      case ("debug", argMap) => {
        debug();
      }
      case _ => {
        OptionParser.help();
      }
    }
  }

  def printTime(title: String, time: Long, pad: Int = 10): Unit = {
    val min = (time / 60000);
    val sec = (time - (min * 60000)) / 1000;
    val ms  = time - (min * 60000) - (sec * 1000);
    System.err.println(title.padTo(pad, ' ') + " : " + min + " min " + sec + " sec " + ms + " ms");
  }

  def debug(): Unit = {
    var prefixtree = PrefixTree[Array[Long]](5);
    var matrix     = Matrix(1316, 1316);
    var charType   = new CharType(new Array[Array[Int]](0), new Array[TokenConfig](0));
    var posInfo    = new PosInfo(Array[String]());
    var metaInfo   = new MetaInfo(Array[Array[String]]());
    def go(): Unit = {
      print("command : ");
      readLine.split(" ").map(_.trim).toList match {
        case "init" :: xs => {
          prefixtree = PrefixTree[Array[Long]](5);
          matrix     = Matrix(1316, 1316);
          charType   = new CharType(new Array[Array[Int]](0), new Array[TokenConfig](0));
          posInfo    = new PosInfo(Array[String]());
          metaInfo   = new MetaInfo(Array[Array[String]]());
        }
        case "load" :: xs => {
          if (xs.isEmpty) {
            prefixtree = PrefixTreeSerializeObject.deserializeFromResource[Array[Long]](Util.dictBin());
            matrix     = Util.kryoDeserializeFromResource[Matrix](Util.matrixBin());
            charType   = Util.kryoDeserializeFromResource[CharType](Util.configBin());
            posInfo    = Util.kryoDeserializeFromResource[PosInfo](Util.posInfoBin());
            metaInfo   = Util.kryoDeserializeFromResource[MetaInfo](Util.metaInfoBin());
          } else {
            val dictDir = xs.head;
            prefixtree = PrefixTreeSerializeObject.deserialize[Array[Long]](Util.dictBin(dictDir));
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
          prefixtree.debugFind(text)(xs => xs.mkString(", "));
        }
        case "search" :: text :: xs => {
          val len = text.length;
          (0 until len).foreach { i =>
            val sub = text.slice(i, len);
            println("%d : %s".format(i + 1, sub));
            prefixtree.prefixSearch(sub).foreach { e =>
              val surface = e._1;
              val data = e._2.mkString(", ");
              println("  %s : %s".format(surface, data));
            }
          }
        }
        case "tokenize" :: text :: xs => {
          val len = text.length;
          val userDict = new LegacyPrefixTree[Long]();
          val viterbi = new Viterbi(prefixtree, matrix, charType, userDict);
          val lattice = viterbi.buildLattice(text);
          println("[0] : BOS");
          (1 to len).foreach { i =>
            println("[%d] : ".format(i));
            lattice(i).foreach { n =>
              val surface = text.slice(n.startIdx, n.endIdx);
              val unk     = if (n.id == -1) " (未)" else "";
              println("  " + n + "\t: "  + surface + unk);
            }
          }
          println("[%d] : EOS".format(len + 1));
        }
        case "analyze" :: text :: xs => {
          val userDict = new LegacyPrefixTree[Long]();
          val midomoji = new Midomoji(prefixtree, matrix, charType, userDict);
          println(midomoji.analyze(text, "detail"));
        }
        case "add" :: surface :: xs => {
          prefixtree.add(surface, 0L) { (existing, elem) =>
            existing match {
              case arr: Array[Long] => {
                val len = arr.length;
                val newArr = new Array[Long](len + 1);
                arr.copyToArray(newArr);
                newArr;
              }
              case _ => Array[Long](elem);
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
          help += "init                     : 辞書をリセットする";
          help += "load [DICT_DIR]          : 辞書を読み込む。DICT_DIRを指定しない場合はresourcesから読み込む";
          help += "cost <LEFT> <RIGHT>      : 連接コストを表示する";
          help += "find <TEXT>              : トライ木に対してSURFACEをキーとする値を取り出す";
          help += "search <TEXT>            : トライ木に対して共通接頭辞検索を行う";
          help += "tokenize <TEXT>          : 未知語ノードの生成を行う";
          help += "analyze <TEXT>           : 形態素解析を行う";
          help += "add <SURFACE>            : トライ木に要素を追加する";
          help += "dump                     : トライ木をダンプする";
          help += "status                   : 辞書のステータスを表示する";
          help += "exit                     : デバッグモードを終了する";
          println(help.mkString("\n"));
        }
      }
      go();
    }
    go();
  }
}
