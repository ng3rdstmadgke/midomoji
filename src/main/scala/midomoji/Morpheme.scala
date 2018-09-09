package com.github.ng3rdstmadgke.midomoji

/**
 * connId  : 12bit
 * genCost : 16bit
 * posId   : 10bit
 * id      : 24bit
 */
object Morpheme {
    def connId(n: Long): Int  = (n & 4095L).toInt;
    def genCost(n: Long): Int = ((n >>> 12) & 65535L).toShort.toInt;
    def posId(n: Long): Int   = ((n >>> 28) & 1023L).toInt;
    def id(n: Long): Int      = ((n >>> 38) & 16777215L).toInt;
    def apply(connId: Long, genCost: Long, posId: Long, id: Long): Long = {
        connId | ((genCost & 65535L) << 12) | ((posId & 1023L) << 28) | ((id & 16777215L) << 38);
    }
}

