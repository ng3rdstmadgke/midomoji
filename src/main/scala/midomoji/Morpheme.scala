package com.github.ng3rdstmadgke.midomoji

/**
 * ipadic の形式
 * 表層形,左文脈ID,右文脈ID,コスト,品詞,品詞細分類1,品詞細分類2,品詞細分類3,活用型,活用形,原形,読み,発音
 */
trait MorphemeBase  {
  val surface: String;
  val leftId: Int;
  val rightId: Int;
  val cost: Int;
  def ==(other: Morpheme): Boolean = surface == other.surface && leftId == other.leftId && rightId == other.rightId;
  override def toString(): String = "(%s, %d, %d)".format(surface, leftId, rightId);
}

case class Morpheme(surface: String, leftId: Int, rightId: Int, cost: Int, pos: String, baseForm: String, yomi: String) extends MorphemeBase;
