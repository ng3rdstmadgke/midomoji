package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;
import scala.collection.mutable.Queue;
import scala.reflect.ClassTag;

class TreeNode[T](val char: Char, val tree: LegacyPrefixTree[T]) {
  override def toString(): String = char.toString;
}
class LegacyPrefixTree[A]() {
  private[this] var nextNodes = new Array[TreeNode[A]](10);
  private[this] var nextSize = 0;
  private[this] var data: List[A] = null;

  def getNextNodes(): Array[TreeNode[A]] = nextNodes;
  def getNextSize(): Int = nextSize;
  def getData(): List[A] = data;

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

  def get(key: Char): TreeNode[A] = {
    def loop(s: Int, e: Int, key: Char, nodes: Array[TreeNode[A]]): TreeNode[A] = {
      if (s > e) {
        null;
      } else {
        val m = (s + e) / 2;
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
    var base  = new Array[Int](50000);
    var check = new Array[Int](50000);
    var data  = new Array[List[A]](50000);
    val q = new Queue[(Int, LegacyPrefixTree[A])]();
    q.enqueue((1, this));
    while (!q.isEmpty) {
      val (currIdx, tree) = q.dequeue();
      val nodes     = tree.getNextNodes();
      val nodesSize = tree.getNextSize();
      val (newBase, shouldExtend) = findBase(nodes, nodesSize, base, check, data);
      base(currIdx) = newBase;
      if (shouldExtend) {
        val size = Math.floor((newBase + 65535) * 1.25).toInt;
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
        base(nextIdx)  = 0;
        check(nextIdx) = currIdx;
        data(nextIdx)  = nodes(i).tree.getData;
        q.enqueue((nextIdx, nodes(i).tree));
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
  def findBase(nodes: Array[TreeNode[A]], nodesSize: Int,
               base: Array[Int], check: Array[Int], data: Array[List[A]]): (Int, Boolean) = {
    val arrLen = base.length;
    var newBase = 1;
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
          newBase += 1;
        }
      }
    (newBase, false);
  }

}

object LegacyPrefixTree {
  def build(morphemePath: String)(parse: (Array[String], Int) => Array[Int]): LegacyPrefixTree[Array[Int]] = {
    Using[Source, LegacyPrefixTree[Array[Int]]](Source.fromFile(morphemePath)) { s =>
      val dict = new LegacyPrefixTree[Array[Int]]();
      s.getLines.zipWithIndex.foreach { lineWithId =>
        val (line, id) = lineWithId;
        val arr = line.split("\t");
        val elem = parse(arr, id);
        dict.add(arr.head, elem);
      }
      dict;
    }
  }

  def check(morphemePath: String, dict: LegacyPrefixTree[Array[Int]]): Unit = {
    Using[Source, Unit](Source.fromFile(morphemePath)) { s =>
      s.getLines.zipWithIndex.foreach { lineWithId =>
        val (line, id) = lineWithId;
        val arr = line.split("\t");
        val data = dict.find(arr.head);
        data match {
          case None    => println(arr.head + " 遷移失敗");
          case Some(d) => {
            if (!d.exists(elem => elem(4) == id)) {
              println(arr.head + " 見つかりませんでした");
            }
          }
        }
      }
    }
  }
}
