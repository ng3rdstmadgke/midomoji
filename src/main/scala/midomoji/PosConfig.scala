package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;

class PosConfig(val pos: Array[String], val katsuyouGata: Array[String], val katsuyouKei: Array[String]) extends Serializable {
  def this() = this(Array[String](), Array[String](), Array[String]());
  def getPos(id: Int): String          =  if (id > 0 && id < pos.length) pos(id) else "UNKNOWN\t*\t*\t*";
  def getkatsuyouGata(id: Int): String =  if (id > 0 && id < katsuyouGata.length) katsuyouGata(id) else "*";
  def getkatsuyouKei(id: Int): String  =  if (id > 0 && id < katsuyouKei.length) katsuyouKei(id) else "*";
}

object PosConfig {
  def build(posPath: String, katsuyouGataPath: String, katsuyouKeiPath: String): PosConfig = {
    val pos          = Using[Source, Array[String]](Source.fromFile(posPath))(s => s.getLines.toArray);
    val katsuyouGata = Using[Source, Array[String]](Source.fromFile(katsuyouGataPath))(s => s.getLines.toArray);
    val katsuyouKei  = Using[Source, Array[String]](Source.fromFile(katsuyouKeiPath))(s => s.getLines.toArray);
    new PosConfig(pos, katsuyouGata, katsuyouKei);
  }
}
