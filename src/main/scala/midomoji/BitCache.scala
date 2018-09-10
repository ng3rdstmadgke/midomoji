package com.github.ng3rdstmadgke.midomoji

class BitCache(private[this] var size: Int) {
  // ノードが存在しない : 0
  // ノードが存在する   : 1
  private[this] var cache = new Array[Long](size / 64 + 1);
  cache(1) = (1L << 1);

  /**
   * cacheを拡張する
   */
  def extendsCache(size: Int): Unit = {
    val tmpCache = new Array[Long](size / 64 + 1);
    cache.copyToArray(tmpCache);
    cache = tmpCache;
  }

  /**
   * 指定されたインデックスに対応するビットを立てる
   */
  def setIndex(idx: Int): Unit = {
    val i = idx >> 6;
    val mask = 1L << (idx & 63);
    cache(i) = cache(i) | mask;
  }

  /**
   * start を開始位置として、空いている一番近いインデックスを返す
   * 最下位の1より前の0の数を数える実装
   */
  def getEmptyIndex(start: Int): Int = {
    val arr = cache;
    val len = arr.length
    var i = start >> 6; // 0 ~ cache.length
    // start よりも前のビットを0埋めするためのマスク
    var mask = -1L << (start & 63);
    while (i < len) {
      // arr(i)を反転(このメソッド内では 1 が空いているインデックス)
      var c = (arr(i) ^ -1L) & mask;
      if (c != 0L) {
        // 最下位の1より前の0の数を数える
        // http://www.nminoru.jp/~nminoru/programming/bitcount.html
        var t = (c & (-c)) - 1;
        t = (t & 0x5555555555555555L) + (t >>> 1  & 0x5555555555555555L);
        t = (t & 0x3333333333333333L) + (t >>> 2  & 0x3333333333333333L);
        t = (t & 0x0F0F0F0F0F0F0F0FL) + (t >>> 4  & 0x0F0F0F0F0F0F0F0FL);
        t = (t & 0x00FF00FF00FF00FFL) + (t >>> 8  & 0x00FF00FF00FF00FFL);
        t = (t & 0x0000FFFF0000FFFFL) + (t >>> 16 & 0x0000FFFF0000FFFFL);
        t = (t & 0x00000000FFFFFFFFL) + (t >>> 32 & 0x00000000FFFFFFFFL);
        return i * 64 + t.toInt;
      }
      mask = -1L;
      i += 1;
    }
    return len * 64;
  }

  /**
   * start を開始位置として、空いている一番近いインデックスを返す
   * 最下位の1のインデックスを2分探索で求める実装
   */
  def getEmptyIndex2(start: Int): Int = {
    val arr = cache;
    val len = arr.length
    var i = start >> 6; // 0 ~ cache.length
    // start よりも前のビットを0埋めするためのマスク
    var mask = -1L << (start & 63);
    while (i < len) {
      // arr(i)を反転(このメソッド内では 1 が空いているインデックス)
      var c = (arr(i) ^ -1L) & mask;
      if (c != 0L) {
        val t = c & (-c); // 一番下の桁の1以外はすべて0にする。
        // 2分探索でビットが立っているインデックスを探す
        var first = 0;
        if ((t & 0x00000000FFFFFFFFL) == 0L) first = 32;
        if ((t & 0x0000FFFF0000FFFFL) == 0L) first += 16;
        if ((t & 0x00FF00FF00FF00FFL) == 0L) first += 8;
        if ((t & 0x0F0F0F0F0F0F0F0FL) == 0L) first += 4;
        if ((t & 0x3333333333333333L) == 0L) first += 2;
        if ((t & 0x5555555555555555L) == 0L) first += 1;
        return i * 64 + first ;
      }
      mask = -1L;
      i += 1;
    }
    return len * 64;
  }


  /**
   * start を開始位置として、空いている一番近いインデックスを返す
   * 素朴な実装
   */
  def getEmptyIndex3(start: Int): Int = {
    val arr = cache;
    val len = arr.length
    var i = start >> 6; // 0 ~ cache.length
    var j = start & 63 // 0 ~ 63
    while (i < len) {
      if (arr(i) != -1L) {
        while (j < 64) {
          if (((arr(i) >>> j) & 1L) == 0L) {
            return i * 64 + j;
          }
          j += 1
        }
      }
      j = 0;
      i += 1;
    }
    return len * 64 + 1;
  }

}
