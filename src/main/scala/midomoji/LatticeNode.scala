package com.github.ng3rdstmadgke.midomoji;

import scala.collection.Iterable;
import scala.collection.AbstractIterator;

/**
 * ラティス構造を構成するノード 
 *
 * @constructor ラティス構造のノードを作る
 * @param startIdx  文字列におけるトークンの始まりのインデックス。(包含)
 * @param endIdx    文字列におけるトークンの終わりのインデックス。(排他)
 * @param leftId    左文脈ID
 * @param rightId   右文脈ID
 * @param genCost   生起コスト
 * @param posId     品詞ID
 * @param id        メタ情報ID
 * @param totalCost EOSまでの最小コスト
 * @param nextNode  totalCostが最小となるノード
 */
class LatticeNode(val startIdx: Int, val endIdx: Int, val morpheme: Long, var totalCost: Int = 0, var nextNode: LatticeNode = null) extends Iterable[LatticeNode] {
  override def iterator: Iterator[LatticeNode] = new LatticeNodeIterator(this.nextNode);

  override def toString(): String = {
    val tpl = "LatticeNode(%d, %d, %d, %d, %d, %d, %d)";
    tpl.format(startIdx, endIdx, Morpheme.connId(morpheme), Morpheme.genCost(morpheme), Morpheme.posId(morpheme), Morpheme.id(morpheme), totalCost);
  }
}

class LatticeNodeIterator(var node: LatticeNode) extends AbstractIterator[LatticeNode] {
  override def next(): LatticeNode = {
    val ret = node;
    node = node.nextNode;
    ret;
  }
  override def hasNext: Boolean = if (node.nextNode == null) false else true;
}
