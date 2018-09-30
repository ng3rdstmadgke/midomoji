package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;
import scala.collection.mutable.{Stack, Queue};
import scala.reflect.ClassTag;

class TreeNode[T](val char: Char, val tree: LegacyPrefixTree[T]) {
  override def toString(): String = char.toString;
}

/**
 * 素朴な実装のトライ木。ノードの探索は2分探索で行う。
 */
class LegacyPrefixTree[A]() {
  private[this] var nextNodes = new Array[TreeNode[A]](10);
  private[this] var nextSize = 0;
  private[this] var data: List[A] = null;

  def getNextNodes(): Array[TreeNode[A]] = nextNodes;
  def getNextSize(): Int = nextSize;
  def getData(): List[A] = data;

  /**
   * ノードの追加
   *
   * @param key   トライ木のキー
   * @param value 格納するデータ
   */
  def add(key: String, value: A): Unit = _add(key, 0, value);

  private def _add(key: String, offset: Int, value: A): Unit = {
    if (offset == key.length) {
      data = if (data == null) List(value) else value :: data;
    } else {
      val tmpNextNode = get(key(offset));
      val nextNode = if (tmpNextNode == null) insert(key(offset)) else tmpNextNode;
      nextNode.tree._add(key, offset + 1, value);
    }
  }

  /**
   * ノードの取り出し
   *
   * @param key トライ木のキー
   */
  def find(key: String): Option[List[A]] = _find(key, 0);

  private def _find(key: String, offset: Int): Option[List[A]] = {
    if (offset == key.length) {
      Option(data);
    } else {
      val node = get(key(offset));
      if (node == null) {
        None;
      } else {
        node.tree._find(key, offset + 1);
      }
    }
  }

  /**
   * ノードの挿入。常にソートされた状態を保つように
   * 挿入ソートを行っている.

   *
   * @param key トライ木のキー
   */
  private def insert(char: Char): TreeNode[A] = {
    if ((nextSize + 1) >= nextNodes.length) extendsArray();
    var i = nextSize;
    val newNode = new TreeNode[A](char, new LegacyPrefixTree[A]());
    nextSize += 1;
    while (i > 0) {
      val node = nextNodes(i - 1);
      if (node.char > char) {
        nextNodes(i) = node;
      } else {
        nextNodes(i) = newNode;
        return newNode;
      }
      i -= 1;
    }
    // i == 0 の場合
    nextNodes(i) = newNode;
    return newNode;
  }

  def extendsArray(): Unit = {
    val newSize = nextSize + 15;
    val tmpNextNodes  = new Array[TreeNode[A]](newSize);
    nextNodes.copyToArray(tmpNextNodes);
    nextNodes = tmpNextNodes;
  }

  /**
   * 2分探索でkeyを探す
   *
   * @param key トライ木のキー
   */
  def get(key: Char): TreeNode[A] = {
    def loop(s: Int, e: Int, key: Char, nodes: Array[TreeNode[A]]): TreeNode[A] = {
      if (s > e) {
        null;
      } else {
        val m = (s + e) >> 1;
        val median = nodes(m);
        if (key < median.char) {
          loop(s, m - 1, key, nodes);
        } else if (key > median.char) {
          loop(m + 1, e, key, nodes);
        } else {
          median;
        }
      }
    }
    loop(0, nextSize - 1, key, nextNodes);
  }

  def toDoubleArray()(implicit tag: ClassTag[A]): PrefixTree[Array[A]] = {
    var base  = new Array[Int](100000);
    var check = new Array[Int](100000);
    var data  = new Array[List[A]](100000);
    val bitCache = new BitCache(100000);
    val s = new Stack[(Int, LegacyPrefixTree[A])]();
    s.push((1, this));
    while (!s.isEmpty) {
      val (currIdx, tree) = s.pop();
      val nodes     = tree.getNextNodes();
      val nodesSize = tree.getNextSize();
      val (newBase, shouldExtend) = findBase(nodes, nodesSize, base, check, data, bitCache);
      base(currIdx) = newBase;
      if (shouldExtend) {
        val size = Math.floor((newBase + 65535) * 1.25).toInt;
        bitCache.extendsCache(size);
        val tmpBase  = new Array[Int](size);
        val tmpCheck = new Array[Int](size);
        val tmpData  = new Array[List[A]](size);
        base.copyToArray(tmpBase);
        check.copyToArray(tmpCheck);
        data.copyToArray(tmpData);
        base  = tmpBase;
        check = tmpCheck;
        data  = tmpData;
      }
      var i = 0;
      while (i < nodesSize) {
        val nextIdx = newBase + nodes(i).char;
        bitCache.setIndex(nextIdx);
        base(nextIdx)  = 0;
        check(nextIdx) = currIdx;
        data(nextIdx)  = nodes(i).tree.getData;
        if (nodes(i).tree.getNextSize > 0) {
          s.push((nextIdx, nodes(i).tree));
        }
        i += 1
      }
    }
    // prefixtreeオブジェクトを返す
    val prefixtree = new PrefixTree[List[A]](base.length, base, check, data);
    prefixtree.convertDataType[Array[A]] { data =>
      data match {
        case null => null;
        case Nil  => null;
        case list => list.toArray;
      }
    }
  }

  // base値を探す
  def findBase(nodes: Array[TreeNode[A]], nodesSize: Int      , base: Array[Int],
               check: Array[Int]        , data: Array[List[A]], bitCache: BitCache): (Int, Boolean) = {
    val arrLen = base.length;
    val firstNodeChar = nodes(0).char.toInt;
    var newBase = bitCache.getEmptyIndex(1 + firstNodeChar) - firstNodeChar;
    var i = 0
      while(i < nodesSize) {
        val newIdx = newBase + nodes(i).char;
        if (newIdx >= arrLen) {
          return (newBase, true);
        }
        if (check(newIdx) == 0) {
          i += 1;
        } else {
          i = 0;
          newBase = bitCache.getEmptyIndex(newBase + 1 + firstNodeChar) - firstNodeChar;
        }
      }
    (newBase, false);
  }

}

object LegacyPrefixTree {
  def build(morphemePath: String): LegacyPrefixTree[Long] = {
    Using[Source, LegacyPrefixTree[Long]](Source.fromFile(morphemePath)) { s =>
      val dict = new LegacyPrefixTree[Long]();
      s.getLines.zipWithIndex.foreach { lineWithId =>
        val (line, id) = lineWithId;
        val Array(surface, left, right, genCost, _*) = line.split("\t");
        dict.add(surface, Morpheme(left.toInt, genCost.toInt, -1, -1));
      }
      dict;
    }
  }

  def check(morphemePath: String, dict: LegacyPrefixTree[Long]): Unit = {
    Using[Source, Unit](Source.fromFile(morphemePath)) { s =>
      s.getLines.zipWithIndex.foreach { lineWithId =>
        val (line, id) = lineWithId;
        val Array(surface, left, right, genCost, posId, _*) = line.split("\t");
        val elem = Morpheme(left.toInt, genCost.toInt, posId.toInt, id.toInt);
        dict.find(surface) match {
          case None    => println(surface + " 遷移失敗");
          case Some(d) => {
            if (!d.exists(e => e == elem)) {
              println(surface + " 見つかりませんでした");
            }
          }
        }
      }
    }
  }
}
