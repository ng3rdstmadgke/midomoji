package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;
import scala.collection.mutable.HashMap;

/**
 * charに対応する文字種IDとトークン生成設定を返す。
 * 
 * @param charTypeName 文字種の名称。KATAKANAとかKANJIとか
 * @param forceUnigram prefixtreeに単語が存在する場合にユニグラムを生成するか(trueなら生成する)。
 * @param groupToken 連続する同一文字種をまとめたトークンを生成するか(trueなら生成する)。
 * @param ngram 何グラムまで生成するか(バイグラムなら2、トリグラムなら3を指定する)。
 * @param tokens 生成するトークンの雛形の配列。Array(leftId, rightId, genCost, posId, id)
 */
case class TokenConfig(charTypeId: Int, charTypeName: String, forceUnigram: Boolean, groupToken: Boolean, ngram: Int, tokens: Array[Array[Int]]) extends Serializable {
  def this() = this(0, "", false, false, 0, Array[Array[Int]]());
}

class CharType(private[this] val charTypeMap: Array[Array[Int]], private[this] val tokenConfigSet: Array[TokenConfig], private[this] val codePointMap: HashMap[(Int, Int), Array[Int]]) extends Serializable {
  val charTypeNum = tokenConfigSet.length;

  def this() = this(Array[Array[Int]](), Array[TokenConfig](), HashMap[(Int, Int), Array[Int]]());

  /**
   * charに対応する文字種IDとトークン生成設定を返す。
   * 一つの文字に対して複数個の設定がある場合がある。
   * 例えば「ー」はHIRAGANAとKATAKANA2つの文字種として扱われる。
   */
  def getTokenConfigs(char: Char): Array[TokenConfig] = {
    charTypeMap(char.toInt).map(charType => tokenConfigSet(charType));
  }

  def getTokenConfigs(codePoint: Int): Array[TokenConfig] = {
    if (codePoint <= 65535) {
      charTypeMap(codePoint).map(charType => tokenConfigSet(charType));
    } else {
      val tmp = new Array[TokenConfig](charTypeNum);
      codePointMap.foreach { e =>
        val (start, end) = e._1;
        val charTypeArr  = e._2;
        if (codePoint >= start && codePoint <= end) {
          charTypeArr.foreach { charType =>
            tmp(charType) = tokenConfigSet(charType);
          }
        }
      }
      val ret = tmp.filter(_ != null);
      if (ret.length == 0) Array(tokenConfigSet(0)) else ret;
    }
  }

  /**
   * 文字種IDに対応するトークン生成設定を返す。
   */
  def getTokenConfig(charType: Int): TokenConfig = {
    tokenConfigSet(charType);
  }

  /**
   * charに対応する文字種IDの配列を返す。
   * 一つの文字に対して複数個のIDがある場合がある。
   * 例えば「ー」はHIRAGANAとKATAKANA2つの文字種として扱われる。
   */
  def getCharTypeIds(char: Char): Array[Int] = {
    charTypeMap(char.toInt);
  }

  def typeIs(char: Char, charType: Int): Boolean = {
    charTypeMap(char.toInt).exists(_ == charType);
  }
}

object CharType {
  def build(charPath: String, charTypePath: String, unkPath: String): CharType = {
    // char.def
    val charArr = Using[Source, Array[(String, Boolean, Boolean, Int)]](Source.fromFile(charPath)) { s =>
      s.getLines.toArray.map{ line =>
        val arr = line.split("\t").map(_.trim);
        (arr(0), arr(1) == "1", arr(2) == "1", arr(3).toInt);
      };
    }
    // 必ず先頭にDEFAULTを設定する
    val defaultIdx = charArr.indexWhere(_._1 == "DEFAULT");
    if (defaultIdx == -1) {
      throw new RuntimeException("DEFAULT char type is not found. add DEFAULT char type to char.def");
    } else {
      val tmp = charArr(0);
      charArr(0) = charArr(defaultIdx);
      charArr(defaultIdx) = tmp;
    }
    // char_type.def
    val charTypeArr = Using[Source, Array[((Int, Int), List[String])]](Source.fromFile(charTypePath)) { s =>
      s.getLines.toArray.map { line =>
        val arr = line.split("\t").map(_.trim);
        val range = arr(0).split("-") match {
          case Array(start)      => (Integer.decode(start).toInt, Integer.decode(start).toInt);
          case Array(start, end) => (Integer.decode(start).toInt, Integer.decode(end).toInt);
          case _                 => (0, -1);
        }
        (range, arr.toList.tail);
      };
    }
    // unk.def
    val unkArr = Using[Source, Array[(String, Array[Int])]](Source.fromFile(unkPath)) { s =>
      s.getLines.toArray.map{ line =>
        val arr = line.split("\t").map(_.trim);
        (arr(0), Array(arr(1).toInt, arr(2).toInt, arr(3).toInt, arr(4).toInt, -1));
      };
    };
    // 文字種名とIDのマップ
    val charMap = charArr.map(_._1).zipWithIndex.foldLeft(Map[String, Int]()) { (map, e) => map + (e._1 -> e._2)};
    // 文字種の数
    val charTypeNum = charArr.length;
    // 同一文字種のトークンをまとめる
    val tokens = Array.fill[List[Array[Int]]](charTypeNum)(Nil);
    unkArr.foreach { e =>
      val (charType, token) = e;
      charMap.get(charType) match {
        case Some(id) => tokens(id) = token :: tokens(id);
        case None     => ();
      }
    }
    // tokenConfigSetの生成
    val tokenConfigSet = (0 until charTypeNum).toArray.map { i =>
      val (charType, forceUnigram, groupToken, ngram) = charArr(i);
      TokenConfig(i, charType, forceUnigram, groupToken, ngram, tokens(i).toArray);
    }

    // charTypeMapの生成
    val charMax = 65536;
    val charTypeMapTmp = Array.fill[List[Int]](charMax)(Nil);
    val codePointMap   = new HashMap[(Int, Int), Array[Int]]();
    charTypeArr.foreach { e =>
      val (range, charTypes) = e;
      val charTypeList = charTypes.map { charType =>
        charMap.get(charType) match {
          case Some(id) => id;
          case None     => -1;
        }
      }.filter(_ > 0);
      // 2バイトに収まる文字
      if (range._1 < charMax) {
        val start = range._1;
        val end   = if (range._2 < charMax) range._2 else charMax - 1 ;
        (start to end).foreach { i =>
          charTypeList.foreach { charType =>
            if (!charTypeMapTmp(i).exists(_ == charType)) {
              charTypeMapTmp(i) = charType :: charTypeMapTmp(i);
            }
          }
        }
      }
      // 2バイトを超える文字
      if (range._2 >= charMax) {
        val cpStart = if (range._1 >= charMax) range._1 else charMax;
        val cpEnd   = range._2;
        codePointMap += ((cpStart, cpEnd) -> charTypeList.toArray);
      }
    }
    val charTypeMap = charTypeMapTmp.map { e =>
      e match {
        case Nil => Array(0);
        case e   => e.toArray;
      }
    }
    new CharType(charTypeMap, tokenConfigSet, codePointMap);
  }
}
