package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;

class PosInfo(val pos: Array[String]) extends Serializable {
  def this() = this(Array[String]());
  def getPos(id: Int): String = if (id > 0 && id < pos.length) pos(id) else "UNKNOWN\t*\t*\t*\t*\t*";
}

object PosInfo {
  def build(posPath: String): PosInfo = {
    val pos = Using[Source, Array[String]](Source.fromFile(posPath))(s => s.getLines.toArray);
    new PosInfo(pos);
  }
}
