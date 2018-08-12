package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;

case class Token(leftId: Int, rightId: Int, cost: Int, pos: Int) extends Serializable {
  def this() = this(-1, -1, -1, -1);
}
case class TokenConfig(charTypeName: String, forceUnigram: Boolean, groupToken: Boolean, ngram: Int, tokens: Array[Token]) extends Serializable {
  def this() = this("", false, false, 0, Array[Token]());
}

class CharType(val charTypeMap: Array[Array[Int]], val tokenConfigSet: Array[TokenConfig]) extends Serializable {
  val charTypeNum = tokenConfigSet.length;

  def this() = this(Array[Array[Int]](), Array[TokenConfig]());

  def getTokenConfigs(char: Char): Array[(Int, TokenConfig)] = {
    charTypeMap(char.toInt).map(charType => (charType, tokenConfigSet(charType)));
  }

  def getTokenConfig(charType: Int): TokenConfig = {
    tokenConfigSet(charType);
  }

  def getCharTypeIds(char: Char): Array[Int] = {
    charTypeMap(char.toInt);
  }
}

object CharType {
  def buildCharType(charDef: String, charTypeDef: String, unkDef: String): CharType = {
    // char.def
    val charArr = Using[Source, Array[(String, Boolean, Boolean, Int)]](Source.fromFile(charDef)) { s =>
      s.getLines.toArray.map{ line =>
        val arr = line.split("\t").map(_.trim);
        (arr(0), arr(1) == "1", arr(2) == "1", arr(3).toInt);
      };
    }
    // char_type.def
    val charTypeArr = Using[Source, Array[((Int, Int), List[String])]](Source.fromFile(charTypeDef)) { s =>
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
    val unkArr = Using[Source, Array[(String, Token)]](Source.fromFile(unkDef)) { s =>
      s.getLines.toArray.map{ line =>
        val arr = line.split("\t").map(_.trim);
        (arr(0), Token(arr(1).toInt, arr(2).toInt, arr(3).toInt, arr(4).toInt));
      };
    };
    // 文字種名とIDのマップ
    val charMap = charArr.map(_._1).zipWithIndex.foldLeft(Map[String, Int]()) { (map, e) => map + (e._1 -> e._2)};
    // 文字種の数
    val charTypeNum = charArr.length;
    // 同一文字種のトークンをまとめる
    val tokens = Array.fill[List[Token]](charTypeNum)(Nil);
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
      TokenConfig(charType, forceUnigram, groupToken, ngram, tokens(i).toArray);
    }

    // charTypeMapの生成
    val charTypeMapTmp = Array.fill[List[Int]](65536)(Nil);
    charTypeArr.foreach { e =>
      val (range, charTypes) = e;
      val charTypeList = charTypes.map { charType =>
        charMap.get(charType) match {
          case Some(id) => id;
          case None     => -1;
        }
      }.filter(_ > 0);
      (range._1 to range._2).foreach { i =>
        if (i > 0 && i < 65536) {
          charTypeList.foreach { charType =>
            if (!charTypeMapTmp(i).exists(_ == charType)) {
              charTypeMapTmp(i) = charType :: charTypeMapTmp(i);
            }
          }
        }
      }
    }
    val charTypeMap = charTypeMapTmp.map { e =>
      e match {
        case Nil => Array(0);
        case e   => e.toArray;
      }
    }
    new CharType(charTypeMap, tokenConfigSet);
  }
}
