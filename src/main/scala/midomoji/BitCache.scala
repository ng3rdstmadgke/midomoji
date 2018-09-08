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
      if ((c & -1L) != 0L) {
        val t = c & (-c); // 一番下の桁の1以外はすべて0にする。
        // 2分探索でビットが立っているインデックスを探す
        var first = 0;
        var last  = 63;
        if ((t & 0xFFFFFFFFL)         == 0L) first = 32                        else last = 31;
        if ((t & 0xFFFF0000FFFFL)     == 0L) first = ((first + last) >> 1) + 1 else last = (first + last) >> 1;
        if ((t & 0xFF00FF00FF00FFL)   == 0L) first = ((first + last) >> 1) + 1 else last = (first + last) >> 1;
        if ((t & 0xF0F0F0F0F0F0F0FL)  == 0L) first = ((first + last) >> 1) + 1 else last = (first + last) >> 1;
        if ((t & 0x3333333333333333L) == 0L) first = ((first + last) >> 1) + 1 else last = (first + last) >> 1;
        if ((t & 0x5555555555555555L) == 0L) first = ((first + last) >> 1) + 1 else last = (first + last) >> 1;
        return i * 64 + first ;
      }
      mask = -1L;
      i += 1;
    }
    return len * 64;
  }

  /**
   * start を開始位置として、空いている一番近いインデックスを返す
   * 素朴な実装(遅い)
   */
  def getEmptyIndex2(start: Int): Int = {
    val arr = cache;
    val len = arr.length
    var i = start >> 6; // 0 ~ cache.length
    var j = start & 63 // 0 ~ 63
    while (i < len) {
      while (j < 64) {
        if (((arr(i) >> j) & 1L) == 0L) {
          return i * 64 + j;
        }
        j += 1
      }
      j = 0;
      i += 1;
    }
    return len * 64 + 1;
  }
}
