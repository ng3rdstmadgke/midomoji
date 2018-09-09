package com.github.ng3rdstmadgke.midomoji

/**
 * connId  : 12bit
 * genCost : 16bit
 * posId   : 10bit
 * id      : 24bit
 */

object Morpheme {
  def connId(n: Long): Int  = (n & 0xFFFL).toInt;
  def genCost(n: Long): Int = ((n >>> 12) & 0xFFFFL).toShort.toInt;
  def posId(n: Long): Int   = ((n >>> 28) & 0x3FFL).toInt;
  def id(n: Long): Int      = ((n >>> 38) & 0xFFFFFFL).toInt;
  def setGenCost(n: Long, genCost: Int) = {
    n & 0xFFFFFFFFF0000FFFL | ((genCost & 0xFFFFL) << 12);
  }
  def apply(connId: Int, genCost: Int, posId: Int, id: Int): Long = {
    (0xFFFL & connId) | ((0xFFFFL & genCost) << 12) | ((0x3FFL & posId) << 28) | ((0xFFFFFFL & id) << 38);
  }
}
