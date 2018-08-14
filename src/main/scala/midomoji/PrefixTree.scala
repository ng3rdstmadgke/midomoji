package com.github.ng3rdstmadgke.midomoji

import scala.reflect.ClassTag;
import scala.collection.AbstractIterator;
import scala.io.Source;

class PrefixTree[A](var size: Int, var base: Array[Int], var check: Array[Int], var data: Array[A])(implicit m: ClassTag[A]) extends Serializable {
  private var none: A = _; // A型のnull値

  /**
   * data を C 型に変更して新しいPrefixTreeオブジェクトを作る
   *
   * @param convert data を C 型に変更するための関数
   * @return key に対応するデータ
   */
  def convertDataType[C](convert: A => C)(implicit c: ClassTag[C]): PrefixTree[C] = {
    val currData = data;
    val newData = c.newArray(size);
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
    for (char <- key) {
      val nextIdx = base(currIdx) + char.toInt;
      if (nextIdx < size && check(nextIdx) == currIdx) {
        currIdx = nextIdx;
      } else {
        return None;
      }
    }
    if (data(currIdx) == null) None else Some(data(currIdx));
  }
  /**
   * key に対応するデータを取得する。
   * 
   * @param char 取得するデータのキー
   * @return key に対応するデータ
   */
  def find(char: Char): Option[A] = {
    val currIdx = 1;
    val nextIdx = base(currIdx) + char.toInt;
    if (nextIdx < size && check(nextIdx) == currIdx && data(nextIdx) != null) {
      Some(data(nextIdx));
    } else {
      None;
    }
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
      val currBase = base(currIdx);
      val tmpNextIdx = currBase + currChar;
      if (tmpNextIdx < size && check(tmpNextIdx) != 0) { // 衝突時
        // 1. currIdx から遷移しているすべてのノード(遷移先ノード)を取得 (index, char)
        val nextNodes = (currBase until PrefixTree.CHAR_MAX + currBase).
          filter(_ < size).
          foldLeft(List[(Int, Int)]()) { (xs, i) =>
            if (check(i) == currIdx) (i, i - currBase) :: xs else xs
          }

        // 2. 遷移先ノードと currChar が遷移可能なbaseを求める
        base(currIdx) = findNewBase((-1, currChar) :: nextNodes);

        nextNodes.foreach { e =>
          val srcIdx  = e._1;
          val srcChar = e._2;
          val srcBase = base(srcIdx);
          val dstIdx  = base(currIdx) + srcChar; // 遷移先ノードの新しいインデックス
          // 3. 遷移先ノードを新しい base で計算した index にコピー
          base(dstIdx)  = base(srcIdx);
          check(dstIdx) = check(srcIdx);
          data(dstIdx)  = data(srcIdx);
          // 4. 旧遷移先ノードから更に遷移しているノードの check を新遷移先ノードの index で更新
          (srcBase until srcBase + PrefixTree.CHAR_MAX).
            filter(_ < size).
            foreach { i => if (check(i) == srcIdx) check(i) = dstIdx; }
          // 5. 旧遷移先ノードの base, check, data をリセット
          base(srcIdx)  = 1;
          check(srcIdx) = 0;
          data(srcIdx)  = stackNone;
        }
      }
      // currChar のノードを追加
      val nextIdx = base(currIdx) + currChar;
      // nextIdx が配列のサイズ以上になってしまった場合は配列を拡張
      if (nextIdx >= size) {
        extendsArray(nextIdx);
      }
      base(nextIdx)  = 1;
      check(nextIdx) = currIdx;
      currIdx = nextIdx;
    }
    // データを登録
    data(currIdx) = func(data(currIdx), value);
  }


  /**
   * すべての遷移先ノードを配置可能な base を求める
   *
   * @param nextNodes 遷移先ノードの配列 (index, charCode)
   * @return すべての遷移先ノードを配置可能な base
   */
  private def findNewBase(nextNodes: List[(Int, Int)]): Int = {
    def go(b: Int, ns: List[(Int, Int)]): Int = {
      ns match {
        case Nil => b;
        case (_, char) :: rest => {
          val newIdx = b + char;
          if (newIdx < size) {
            if (check(newIdx) == 0) {
              go(b, rest);
            } else {
              go(b + 1, nextNodes);
            }
          } else {
            // newIdx が配列のサイズ以上になってしまった場合は配列を拡張
            extendsArray(newIdx);
            go(b, rest);
          }
        }
      }
    }
    go(1, nextNodes);
  }

  /**
   * base, check, data を引数のsizeの1.25倍に拡張する。
   *
   * @param newSizeBase 拡張後のサイズの基準値
   */
   private def extendsArray(newSizeBase: Int): Unit = {
     size = Math.floor(newSizeBase * 1.25).toInt;
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
  def dump: Unit = {
    println("size = %d".format(size));
    var sepLine = "+----------+----------+----------+----------+";
    println(sepLine);
    println("|index     |base      |check     |char, data|");
    println(sepLine);
    println("|1         " + "|" + base(1).toString.padTo(10, ' ') + "|" + check(1).toString.padTo(10, ' ') + "|");
    println(sepLine);
    val elems = (2 until size).
      filter(i => check(i) != 0).
      foreach { i =>
        val d = if (data(i) != null && data(i) != Nil) data(i).toString else "";
        val cp = i - base(check(i));
        val char = cp.toChar;
        println("|" + i.toString.padTo(10, ' ') + "|" + base(i).toString.padTo(10, ' ') + "|" + check(i).toString.padTo(10, ' ') + "|" + char + "(" + cp + "), " + d);
        println(sepLine);
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
    Using[Source, PrefixTree[Array[A]]](Source.fromFile(morphemePath)) { s =>
      val pt = PrefixTree[List[A]](700000);
      s.getLines.zipWithIndex.foreach { lineWithId =>
        val (line, id) = lineWithId;
        val arr = line.split("\t");
        val surface = arr.head;
        val elem = parse(arr, id);
        pt.add[A](surface, elem)(add);
      }
      pt.convertDataType[Array[A]] { data =>
        data match {
          case ls @ x :: xs => ls.toArray;
          case _            => null;
        }
      }
    }
  }

  /**
   * morphemePath の形態素がprefixtreeにすべて登録されているかチェックするメソッド
   *
   * @param prefixree テスト対象のトライ木
   * @param morphemePath トライ木に登録した辞書ファイル
   * @param parse 辞書ファイルの行を行をパースして、トライ木に登録してあるオブジェクトに変換する関数
   * @param exists トライ木の検索結果の中に、parseで生成したオブジェクトが含まれているかチェックする関数(含んでいればtrue)
   */
  def check[A](prefixree: PrefixTree[Array[A]], morphemePath: String)(parse: (Array[String], Int) => A)(exists: (A, Array[A]) => Boolean): Unit = {
    var errors = Using[Source, List[String]](Source.fromFile(morphemePath)) { s =>
      s.getLines.zipWithIndex.foldLeft(List[String]()) { (es, lineWithId) =>
        val (line, id) = lineWithId;
        val msg = "input : " + line + "\noutput : ";
        val arr = line.split("\t");
        val surface = arr.head;
        val elem = parse(arr, id);
        prefixree.find(surface) match {
          case None     => msg + "None" :: es;
          case Some(ds) => if (exists(elem, ds)) es  else msg + ds.length :: es;
        }
      }
    }
    errors match {
      case Nil => println("OK!!!");
      case ls  => println(ls.mkString("\n"));
    }
  }
}

class PrefixSearchIterator[A](private[this] val key: String      , private[this] val size: Int     , private[this] val base: Array[Int],
                              private[this] val check: Array[Int], private[this] val data: Array[A]) extends AbstractIterator[(String, A)] {
  var currIdx = 1;
  var keyIdx = 0;
  val keyLen = key.length;

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
