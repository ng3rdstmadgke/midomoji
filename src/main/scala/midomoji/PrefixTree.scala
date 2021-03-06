package com.github.ng3rdstmadgke.midomoji

import scala.reflect.ClassTag;
import scala.collection.AbstractIterator;
import scala.io.Source;

class PrefixTree[A](private[this] var size: Int,
                    private[this] var base: Array[Int],
                    private[this] var check: Array[Int],
                    private[this] var data: Array[A])(implicit m: ClassTag[A]) extends Serializable {
  private var none: A = _; // A型のnull値

  def getFields: (Int, Array[Int], Array[Int], Array[A]) = (size, base, check, data);

  /**
   * data を C 型に変更して新しいPrefixTreeオブジェクトを作る
   *
   * @param convert data を C 型に変更するための関数
   * @return key に対応するデータ
   */
  def convertDataType[C](convert: A => C)(implicit c: ClassTag[C]): PrefixTree[C] = {
    optimize;
    val newData = c.newArray(size);
    val currData = data;
    (0 until size).foreach ( i => newData(i) = convert(currData(i)));
    new PrefixTree[C](size, base, check, newData);
  }

  /**
   * key に対応するデータを取得する。
   * 
   * @param key 取得するデータのキー
   * @return key に対応するデータ
   */
  def find(key: String): Option[A] = {
    var currIdx = 1;
    var seek = 0;
    val len = key.length;
    while (seek < len) {
      val nextIdx = base(currIdx) + key(seek).toInt;
      if (nextIdx >= size || check(nextIdx) != currIdx) {
        return None;
      }
      currIdx = nextIdx;
      seek += 1;
    }
    if (data(currIdx) == null) None else Some(data(currIdx));
  }

  /**
   * charがトライ木に登録されているかどうかを確かめる
   * 
   * @param char 
   * @return 登録されていればtrue, そうでなければfalse
   */
  def exists(char: Char): Boolean = {
    val currIdx = 1;
    val nextIdx = base(currIdx) + char.toInt;
    if (nextIdx < size && check(nextIdx) == currIdx && data(nextIdx) != null) {
      true;
    } else {
      false;
    }
  }

  /**
   * keyがトライ木に登録されているかどうかを確かめる
   * 
   * @param key
   * @return 登録されていればtrue, そうでなければfalse
   */
  def exists(text: String, startIdx: Int, endIdx: Int): Boolean = {
    var currIdx = 1;
    var seek    = startIdx;
    while (seek < endIdx) {
      val nextIdx = base(currIdx) + text(seek);
      if (nextIdx < size && check(nextIdx) == currIdx) {
        currIdx = nextIdx;
        seek += 1;
      } else {
        return false;
      }
    }
    if (data(currIdx) == null) false else true;
  }

  /**
   * key に前方一致するデータを取得する
   * 
   * @param key 取得するデータのキー
   * @return key に前方一致するデータを取り出すためのイテレータ
   */
  def prefixSearch(key: String): PrefixSearchIterator[A] = new PrefixSearchIterator[A](key, size, base, check, data);

  /**
   * トライ木にデータを登録する
   * 
   * @param key 登録対象のデータのキー
   * @param value 登録対象のデータ
   * @param func dataに新しいデータを追加する関数。
   *             既存データと追加データを引数にとり、新しくdataに格納するデータを返す
   */
  def add[B](key: String, value: B)(func: (A, B) => A): Unit = {
    // よく使うフィールドをスタック変数として定義
    val stackNone = none;
    var (currIdx, keyIdx) = findFaildPoint(key);
    for (i <- (keyIdx until key.length)) {
      val currChar = key(i).toInt;
      val tmpNextIdx = base(currIdx) + currChar;
      if (base(currIdx) == 0) {            // 遷移先ノードが存在しない場合
        val newBase = findNewBase(currChar);
        base(currIdx) = newBase;
      } else if (tmpNextIdx >= size) {     // 遷移先インデックスが配列のサイズを超過したとき
        extendsArray(tmpNextIdx);
      } else if (check(tmpNextIdx) != 0) { // 衝突時
        val currBase = base(currIdx);
        // 遷移先ノードがある場合
        // 1. currIdx から遷移しているすべてのノード(遷移先ノード)を取得 (charCodeのリストを作る)
        val nextNodes = (currBase until Math.min(PrefixTree.CHAR_MAX + currBase, size)).
          foldLeft(List[Int]()) { (xs, i) =>
            if (check(i) == currIdx) i - currBase :: xs else xs
          }
        // 2. 遷移先ノードと currChar が遷移可能なbaseを求める
        val newBase = findNewBase(currChar :: nextNodes);
        base(currIdx) = newBase;

        nextNodes.foreach { srcChar =>
          val srcIdx  = srcChar + currBase;
          val srcBase = base(srcIdx);
          val dstIdx  = base(currIdx) + srcChar; // 遷移先ノードの新しいインデックス
          // 3. 遷移先ノードを新しい base で計算した index にコピー
          base(dstIdx)  = base(srcIdx);
          check(dstIdx) = check(srcIdx);
          data(dstIdx)  = data(srcIdx);
          // 4. 旧遷移先ノードから更に遷移しているノードの check を新遷移先ノードの index で更新
          (srcBase until Math.min(srcBase + PrefixTree.CHAR_MAX, size)).
            foreach { i => if (check(i) == srcIdx) check(i) = dstIdx; }
          // 5. 旧遷移先ノードの base, check, data をリセット
          base(srcIdx)  = 0;
          check(srcIdx) = 0;
          data(srcIdx)  = stackNone;
        }
      }
      // currChar のノードを追加
      val nextIdx = base(currIdx) + currChar;
      base(nextIdx)  = 0;
      check(nextIdx) = currIdx;
      currIdx = nextIdx;
    }
    // データを登録
    data(currIdx) = func(data(currIdx), value);
  }

  /**
   * すべての遷移先ノードを配置可能な base を求める
   *
   * @param nextChar 遷移先の文字コード (charCode)
   * @return すべての遷移先ノードを配置可能な base
   */
  private def findNewBase(nextChar: Int): Int = {
    var stackSize  = size;
    var stackCheck = check;
    var newBase = 1;
    while (true) {
      val newIdx = newBase + nextChar;
      if (newIdx >= stackSize) {
        // newIdx が配列のサイズ以上になってしまった場合は配列を拡張
        extendsArray(newIdx);
        stackCheck = check;
        stackSize  = size;
      }
      if (stackCheck(newIdx) == 0) {
        return newBase;
      }
      newBase += 1;
    }
    return newBase;
  }

  /**
   * すべての遷移先ノードを配置可能な base を求める
   *
   * @param nextNodes 遷移先ノードの配列 (charCode)
   * @return すべての遷移先ノードを配置可能な base
   */
  private def findNewBase(nextNodes: List[Int]): Int = {
    var stackSize  = size;
    var stackCheck = check;
    var rest = nextNodes;
    var newBase = 1;
    while (rest != Nil) {
      val newIdx = newBase + rest.head;
      if (newIdx >= stackSize) {
        // newIdx が配列のサイズ以上になってしまった場合は配列を拡張
        extendsArray(newIdx);
        stackCheck = check;
        stackSize  = size;
      }
      if (stackCheck(newIdx) == 0) {
        rest = rest.tail;
      } else {
        rest = nextNodes;
        newBase += 1;
      }
    }
    newBase;
  }

  /**
   * base, check, data を引数のsizeの1.25倍に拡張する。
   *
   * @param newSizeBase 拡張後のサイズの基準値
   */
  private def extendsArray(newSizeBase: Int): Unit = {
    val newSize = Math.floor(newSizeBase * 1.25).toInt;
    changeSize(newSize);
  }

  def optimize(): Unit = {
    val lastIdx = (0 until size).reverse.find(i => check(i) != 0);
    lastIdx match {
      case None    => changeSize(2);
      case Some(i) => changeSize(i + 1);

    }
  }

  private def changeSize(newSize: Int): Unit = {
    size = newSize;
    val tmpBase  = new Array[Int](size);
    val tmpCheck = new Array[Int](size);
    val tmpData  = m.newArray(size);
    base.copyToArray(tmpBase);
    check.copyToArray(tmpCheck);
    data.copyToArray(tmpData);
    base  = tmpBase;
    check = tmpCheck;
    data  = tmpData;
  }


  /**
   * 遷移に失敗した時点のindexと文字を返す
   * 
   * @param key 探索する文字列
   * @return 遷移終了時のindexと遷移失敗時のkeyのインデックス
   */
  private def findFaildPoint(key: String): (Int, Int) = {
    var currIdx = 1;
    val len = key.length;
    for (i <- (0 until len)) {
      val nextIdx = base(currIdx) + key(i).toInt;
      if (nextIdx < size && check(nextIdx) == currIdx) {
        currIdx = nextIdx;
      } else {
        return (currIdx, i);
      }
    }
    (currIdx, len);
  }

  /**
   * base, check, data に現在格納されている値を出力する。
   */
  def dump(begin: Int = 0, end: Int = -1): Unit = {
    val beginIdx = if (begin < 0) 0 else begin;
    val endIdx   = if (end < 0 || end > size) size else end;
    var sepLine = "+----------+----------+----------+----------+";
    println(sepLine);
    println("|index     |base      |check     |char, data|");
    println(sepLine);
    println("|1         " + "|" + base(1).toString.padTo(10, ' ') + "|" + check(1).toString.padTo(10, ' ') + "|");
    println(sepLine);
    (beginIdx until endIdx).
      filter(i => check(i) != 0).
      foreach { i =>
        val d = if (data(i) != null && data(i) != Nil) data(i).toString else "";
        val cp = i - base(check(i));
        val char = cp.toChar;
        println("|" + i.toString.padTo(10, ' ') + "|" + base(i).toString.padTo(10, ' ') + "|" + check(i).toString.padTo(10, ' ') + "|" + char + "(" + cp + "), " + d);
        println(sepLine);
      }
    println("size = %d".format(size));
  }

  def debugFind(key: String)(dataToString: A => String): Unit = {
    var currIdx = 1;
    var sepLine = "+----------+----------+----------+----------+";
    println(sepLine);
    println("|index     |base      |check     |char, data|");
    println(sepLine);
    println("|1         " + "|" + base(1).toString.padTo(10, ' ') + "|" + check(1).toString.padTo(10, ' ') + "|");
    println(sepLine);
    for (char <- key) {
      val cp = char.toInt;
      val nextIdx = base(currIdx) + cp;
      val d = if (data(nextIdx) != null && data(nextIdx) != Nil) data(nextIdx).toString else "";
      println("|" + nextIdx.toString.padTo(10, ' ') + "|" + base(nextIdx).toString.padTo(10, ' ') + "|" + check(nextIdx).toString.padTo(10, ' ') + "|" + char + "(" + char.toInt + "), " + d);
      println(sepLine);
      if (nextIdx < size && check(nextIdx) == currIdx) {
        currIdx = nextIdx;
      } else {
        println(char + "(" + cp + ") : 遷移に失敗しました。");
        return ();
      }
    }
    if (data(currIdx) == null) {
      println(key + " : null");
      println("遷移には成功しましたが、データはありません。");
    } else {
      println(key + " : " + dataToString(data(currIdx)));
      println("遷移に成功しました。");
    }
    return ();
  }

  def status: Unit = {
    println("----- prefixtree -----");
    println("size=%d".format(this.size));
    val lastIdx = (0 until size).reverse.find(i => check(i) != 0);
    lastIdx match {
      case None    => println("node is not found");
      case Some(i) => println("last node : index=%d, base=%d, check=%d, data=%s".format(i, base(i), check(i), data(i)));
    }
  }
}

object PrefixTree {
  val CHAR_MAX = 65536;
  def apply[A](size: Int)(implicit m: ClassTag[A]): PrefixTree[A] = {
    val base: Array[Int]  = new Array[Int](size);
    val check: Array[Int] = new Array[Int](size);
    val data: Array[A]    = m.newArray(size);
    base(1) = 1;
    new PrefixTree[A](size, base, check, data);
  }

  /**
   * 辞書ファイルからトライ木を構築するメソッド
   *
   * @param morphemePath 辞書ファイル
   * @param parse 辞書ファイルの行を行をパースして、トライ木に登録するオブジェクトに変換する関数
   * @param add PrefixTree#addで利用する関数
   */
  def build[A](morphemePath: String)(parse: (Array[String], Int) => A)(add: (List[A], A) => List[A])(implicit m: ClassTag[A]): PrefixTree[Array[A]] = {
    val prefixtree = Using[Source, PrefixTree[List[A]]](Source.fromFile(morphemePath)) { s =>
      val pt = PrefixTree[List[A]](700000);
      s.getLines.zipWithIndex.foreach { lineWithId =>
        val (line, id) = lineWithId;
        val arr = line.split("\t");
        val surface = arr.head;
        val elem = parse(arr, id);
        pt.add[A](surface, elem)(add);
        if (id % 10000 == 0) println(id + " words...");
      }
      pt;
    }
    prefixtree.convertDataType[Array[A]] { data =>
      data match {
        case null => null;
        case Nil  => null;
        case list => list.toArray;
      }
    }
  }

  /**
   * morphemePath の形態素がprefixtreeにすべて登録されているかチェックするメソッド
   *
   * @param prefixree テスト対象のトライ木
   * @param morphemePath トライ木に登録した辞書ファイル
   */
  def check(prefixree: PrefixTree[Array[Long]], morphemePath: String): Unit = {
    Using[Source, Unit](Source.fromFile(morphemePath)) { s =>
      var success = true;
      s.getLines.zipWithIndex.foreach { lineWithId =>
        val (line, id) = lineWithId;
        val Array(surface, left, right, genCost, posId, _*) = line.split("\t");
        val elem = Morpheme(left.toInt, genCost.toInt, posId.toInt, id.toInt);
        prefixree.find(surface) match {
          case None     => {
            println(surface + " : 遷移失敗");
            success = false;
          }
          case Some(ds) => {
            if (!ds.exists(e => e == elem)) {
              println(surface + " : 見つかりませんでした");
              success = false;
            }
          }
        }
      }
      if (success) println("OK!!!");
    }
  }
}

class PrefixSearchIterator[A](private[this] val key: String      , private[this] val size: Int     , private[this] val base: Array[Int],
                              private[this] val check: Array[Int], private[this] val data: Array[A]) extends AbstractIterator[(String, A)] {
  private[this] var currIdx = 1;
  private[this] var keyIdx = 0;
  private[this] val keyLen = key.length;

  override def next(): (String, A) = (key.substring(0, keyIdx), data(currIdx));

  override def hasNext: Boolean = {
    while (keyIdx < keyLen) {
      val nextIdx = base(currIdx) + key(keyIdx).toInt;
      if (nextIdx < size && currIdx == check(nextIdx)) {
        currIdx = nextIdx;
        keyIdx += 1;
        if (data(nextIdx) != null) {
          return true;
        }
      } else {
        return false;
      }
    }
    false;
  }
}
