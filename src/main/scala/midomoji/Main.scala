package com.github.ng3rdstmadgke.midomoji;

import scala.io.Source;
import scala.io.StdIn.readLine;
import scala.collection.mutable.ListBuffer;
import java.text.Normalizer;
import java.io.{FileOutputStream, FileInputStream, InputStream};

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

  def help(): Unit = {
    val help = """
[ usage ]
  java -jar midomoji.jar [COMMAND]

  [ COMMAND ]
    - analyze [TARGET_FILE] [options]    : 標準入力またはファイルで与えられた文字列を形態素解析する

      [TARGET_FILE] :  解析対象のファイル

      [options]
        -f | --format <simple | detail | wakati | wakati-base>
          simple      : 表層文字列,左文脈ID,右文脈ID,生起コスト をタブ区切りで表示する
          detail      : 表層文字列,左文脈ID,右文脈ID,生起コスト,品詞情報,原型,読み をタブ区切りで表示する
          wakati      : 表層文字列 を空白区切りで表示する
          wakati-base : 原型 を空白区切りで表示する

    - build-dict <DICT_DIR>      : 形態素辞書を構築する
    - build-matrix <DICT_DIR>    : 連接コスト表を構築する
    - build-config <DICT_DIR>    : 未知語生成設定を構築する
    - build-pos-info <DICT_DIR>  : 形態素の品詞情報表を構築する
    - build-meta-info <DICT_DIR> : 形態素のメタ情報表(原型や読みなど)を構築する
    - check-dict <DICT_DIR>      : 構築した形態素辞書にすべての形態素が登録されているかチェックする
    - check-matrix <DICT_DIR>    : 構築した連接コスト表にすべての連接コストが登録されているかをチェックする
    - debug <DICT_DIR>           : デバッグ用コマンドラインを起動する
""";
    println(help.trim);
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
        PrefixTree.check[Array[Int]](prefixtree, Util.morphemeTsv(dictDir))(parse)(exists);
      }
      case ("check-matrix", argMap) if argMap.contains("dict-dir") => {
        val dictDir = argMap("dict-dir");
        val matrix = Util.kryoDeserialize[Matrix](Util.matrixBin(dictDir));
        Matrix.check(matrix, Util.matrixTsv(dictDir));
      }
      case ("debug", argMap) if argMap.contains("dict-dir") => {
        debug(argMap("dict-dir"));
      }
      case ("analyze", argMap) => {
        val prefixtree = PrefixTreeSerializeObject.deserializeFromResource[Array[Array[Int]]](Util.dictBin());
        val matrix     = Util.kryoDeserializeFromResource[Matrix](Util.matrixBin());
        val charType   = Util.kryoDeserializeFromResource[CharType](Util.configBin());
        val midomoji = new Midomoji(prefixtree, matrix, charType);
        argMap.get("target") match {
          case None         => {
            argMap.get("format") match {
              case Some(fmt) => midomoji.analyzeStdin(Midomoji.format(fmt));
              case _         => midomoji.analyzeStdin(Midomoji.format("simple"));
            }
          }
          case Some(target) => {
            argMap.get("format") match {
              case Some(fmt) => midomoji.analyzeFile(target)(Midomoji.format(fmt));
              case _         => midomoji.analyzeFile(target)(Midomoji.format("simple"));
            }
          }
        }
      }
      case _ => {
        help();
      }
    }
  }

  def debug(dictDir: String): Unit = {
    var prefixtree = PrefixTreeSerializeObject.deserialize[Array[Array[Int]]](Util.dictBin(dictDir));
    var matrix     = Util.kryoDeserialize[Matrix](Util.matrixBin(dictDir));
    var charType   = Util.kryoDeserialize[CharType](Util.configBin(dictDir));
    var posInfo    = Util.kryoDeserialize[PosInfo](Util.posInfoBin(dictDir));
    var metaInfo   = Util.kryoDeserialize[MetaInfo](Util.metaInfoBin(dictDir));
    def go(): Unit = {
      print("command : ");
      readLine.split(" ").map(_.trim).toList match {
        case "init" :: xs => {
          prefixtree = PrefixTree[Array[Array[Int]]](5);
          matrix     = Matrix(1316, 1316);
          charType   = new CharType(new Array[Array[Int]](0), new Array[TokenConfig](0));
          posInfo    = new PosInfo(Array[String]());
          metaInfo   = new MetaInfo(Array[Array[String]]());
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
          val midomoji = new Midomoji(prefixtree, matrix, charType);
          val str = midomoji.analyze(text).map { n =>
            val pos = posInfo.getPos(n.posId);
            val base = metaInfo.getBaseForm(n.id, n.surface);
            val yomi = metaInfo.getYomi(n.id, n.surface);
            "%s\t%d\t%d\t%d\t%s\t%s\t%s".format(n.surface, n.leftId, n.rightId, n.genCost, pos, base, yomi);
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
          help += "init                                 : 辞書をリセットする";
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
