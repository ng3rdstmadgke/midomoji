package com.github.ng3rdstmadgke.midomoji;

object OptionParser {
  def parse(argsArr: Array[String]): (String, Map[String, String]) = {
    def _parse(args: List[String], ret: Map[String, String]): (String, Map[String, String]) = {
       args match {
        case "build-dict"      :: rest => ("build-dict"     , buildOptionParser(rest));
        case "build-matrix"    :: rest => ("build-matrix"   , buildOptionParser(rest));
        case "build-config"    :: rest => ("build-config"   , buildOptionParser(rest));
        case "build-pos-info"  :: rest => ("build-pos-info" , buildOptionParser(rest));
        case "build-meta-info" :: rest => ("build-meta-info", buildOptionParser(rest));
        case "check-dict"      :: rest => ("check-dict"     , buildOptionParser(rest));
        case "check-matrix"    :: rest => ("check-matrix"   , buildOptionParser(rest));
        case "check-user-dict" :: rest => ("check-user-dict", buildOptionParser(rest));
        case "analyze"         :: rest => ("analyze"        , analyzeOptionParser(rest));
        case "debug"           :: rest => ("debug"          , Map[String, String]());
        case _                         => ("help"           , Map[String, String]());
      }
    }
    _parse(argsArr.map(_.trim).toList, Map[String, String]());
  }

  def buildOptionParser(args: List[String]): Map[String, String] = {
    def go(args: List[String], ret: Map[String, String]): Map[String, String] = {
      args match {
        case Nil => ret;
        case some :: rest => {
          if (!ret.contains("dict-dir")) {
            go(rest, ret + ("dict-dir" -> some));
          } else {
            go(rest, ret);
          }
        }
      }
    }
    go(args, Map[String, String]());
  }

  def analyzeOptionParser(args: List[String]): Map[String, String] = {
    def go(args: List[String], ret: Map[String, String]): Map[String, String] = {
      args match {
        case Nil => ret;
        case ("-f" | "--format") :: format :: rest => {
           go(rest, ret + ("format" -> format));
        }
        case ("-o" | "--output") :: output :: rest => {
           go(rest, ret + ("output" -> output));
        }
        case ("-u" | "--user-dict") :: userDict :: rest => {
           go(rest, ret + ("user-dict" -> userDict));
        }
        case ("-bs" | "--buffer-size") :: bufferSize :: rest => {
           go(rest, ret + ("buffer-size" -> bufferSize));
        }
        case some :: rest => {
          if (!ret.contains("input")) {
            go(rest, ret + ("input" -> some));
          } else {
            go(rest, ret);
          }
        }
      }
    }
    go(args, Map[String, String]());
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
    - check-user-dict <DICT_DIR> : ユーザー辞書用のトライ木に形態素辞書の単語を全部登録して、登録されているかをチェックする;
    - debug <DICT_DIR>           : デバッグ用コマンドラインを起動する
""";
    println(help.trim);
  }
}
