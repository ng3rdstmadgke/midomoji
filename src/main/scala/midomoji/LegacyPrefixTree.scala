package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;

class TreeNode[T](val char: Char, val tree: LegacyPrefixTree[T]) {
  override def toString(): String = char.toString;
}
class LegacyPrefixTree[A]() {
  private[this] var nextNodes = new Array[TreeNode[A]](10);
  private[this] var nextSize = 0;
  private[this] var data: List[A] = null;

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
