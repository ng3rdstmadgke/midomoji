package com.github.ng3rdstmadgke.midomoji

import scala.reflect.ClassTag;
import scala.collection.AbstractIterator;
import java.io.{OutputStream, InputStream, ObjectOutputStream, ObjectInputStream};
import java.nio.file.{Paths, Files};

@SerialVersionUID(1L)
class PrefixTree[A] private (var size: Int, val equals: (A, A) => Boolean)(implicit m: ClassTag[List[A]]) extends Serializable {
  private[this] var base: Array[Int]     = new Array[Int](size);
  private[this] var check: Array[Int]    = new Array[Int](size);
  private[this] var data: Array[List[A]] = m.newArray(size);
  base(1) = 1;

  def serialize(dictPath: String): Unit = {
    Using[OutputStream, Unit](Files.newOutputStream(Paths.get(dictPath))) { os =>
      Using[ObjectOutputStream, Unit](new ObjectOutputStream(os)) { oos =>
        oos.writeObject(this);
      }
    }
  }

  /**
   * key に対応するデータを取得する。
   * 
   * @param key 取得するデータのキー
   * @return key に対応するデータ
   */
  def find(key: String): Option[List[A]] = {
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
   */
  def add(key: String, value: A): Unit = {
    var (currIdx, keyIdx) = findFaildPoint(key);
    for (i <- (keyIdx until key.length)) {
      val currChar = key(i).toInt;
      val tmpNextIdx = base(currIdx) + currChar;
      if (tmpNextIdx < size && check(tmpNextIdx) != 0) { // 衝突時
        // 1. currIdx から遷移しているすべてのノード(遷移先ノード)を取得 (index, char)
        val nextNodes = (currIdx until PrefixTree.CHAR_MAX + currIdx).
          filter(i => i < size).
          foldLeft(List[(Int, Int)]()) { (xs, i) =>
            if (check(i) == currIdx) (i, i - base(currIdx)) :: xs else xs
          }

        // 2. 遷移先ノードと currChar が遷移可能なbaseを求める
        base(currIdx) = findNewBase((-1, currChar) :: nextNodes);

        nextNodes.foreach { e =>
          val srcIdx  = e._1;
          val srcChar = e._2;
          val dstIdx  = base(currIdx) + srcChar;
          val srcBase = base(srcIdx);
          // 3. 遷移先ノードを新しい base で計算した index にコピー
          base(dstIdx)  = base(srcIdx);
          check(dstIdx) = check(srcIdx);
          data(dstIdx)  = data(srcIdx);
          // 4. 旧遷移先ノードから更に遷移しているノードの check を新遷移先ノードの index で更新
          (srcBase until srcBase + PrefixTree.CHAR_MAX).filter(i => i < size).foreach { i => if (check(i) == srcIdx) check(i) = dstIdx; }
          // 5. 旧遷移先ノードの base, check, data をリセット
          base(srcIdx)  = 1;
          check(srcIdx) = 0;
          data(srcIdx)  = null;
        }
      }
      // currChar のノードを追加
      val nextIdx = base(currIdx) + currChar;
      // nextIdx が配列のサイズ以上になってしまった場合は配列を拡張
      if (size < nextIdx) {
        extendsArray(nextIdx);
      }
      base(nextIdx)  = 1;
      check(nextIdx) = currIdx;
      currIdx = nextIdx;
    }
    // 重複したデータがなければ登録
    data(currIdx) = if (data(currIdx) == null) {
      List[A](value);
    } else {
      data(currIdx).find(equals(_, value)) match {
        case None => value :: data(currIdx);
        case _ => data(currIdx);
      }
    }
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
          if (check(newIdx) == 0) {
            go(b, rest);
          } else if (newIdx < size) {
            go(b + 1, nextNodes);
          } else {
            // newIdx が配列のサイズ以上になってしまった場合は配列を拡張
            extendsArray(newIdx);
            go(b, ns);
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
  def apply[A](size: Int): PrefixTree[A] = new PrefixTree(size, (e1: A, e2: A) => e1 == e2);
  def apply[A](dictPath: String): PrefixTree[A] = {
    Using[InputStream, PrefixTree[A]](Files.newInputStream(Paths.get(dictPath))) { is =>
      Using[ObjectInputStream,PrefixTree[A]](new ObjectInputStream(is)) { ois =>
        ois.readObject().asInstanceOf[PrefixTree[A]];
      }
    }
  }
}

class PrefixSearchIterator[A](key: String, size: Int,  base: Array[Int], check: Array[Int], data: Array[List[A]]) extends AbstractIterator[(String, List[A])] {
  var currIdx = 1;
  var keyIdx = 0;
  val keyLen = key.length;

  override def next(): (String, List[A]) = (key.substring(0, keyIdx), data(currIdx));

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
