package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;

/**
 * 形態素の原型とよみを格納する。
 */
class TokenMetaInfo(private[this] val metaData: Array[Array[String]]) extends Serializable {
  def this() = this(Array[Array[String]]());
  def getBaseForm(id: Int, default: String): String = {
    if (id > 0 && id < metaData.length) {
      val baseForm = metaData(id)(0);
      if (baseForm == null) default else baseForm;
    } else {
      default;
    }
  }

  def getYomi(id: Int, default: String): String = {
    if (id > 0 && id < metaData.length) {
      val yomi = metaData(id)(1);
      if (yomi == null) default else yomi;
    } else {
      default;
    }
  }
}

object TokenMetaInfo {
  def build(morphemePath: String): TokenMetaInfo = {
    val metaData = Using[Source, Array[Array[String]]](Source.fromFile(morphemePath)) { s =>
      s.getLines.toArray.map { line =>
        val Array(surface, left, right, genCost, posId, baseForm, yomi, pron) = line.split("\t");
        val elem1 = if (surface == baseForm) null else baseForm;
        val elem2 = if (surface == yomi) null else yomi;
        Array(elem1, elem2);
      }
    }
    new TokenMetaInfo(metaData);
  }
}
