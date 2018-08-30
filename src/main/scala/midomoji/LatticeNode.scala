package com.github.ng3rdstmadgke.midomoji;

import scala.collection.Iterable;
import scala.collection.AbstractIterator;

class LatticeNode(val startIdx: Int  , val endIdx: Int, val leftId: Int , val rightId: Int,
                  val genCost: Int   , val posId: Int , val id: Int     , val nextIdx: Int,
                  var totalCost: Int = 0, var nextNode: LatticeNode = null) extends Iterable[LatticeNode] {
  override def iterator: Iterator[LatticeNode] = new LatticeNodeIterator(this.nextNode);

  override def toString(): String = {
    val tpl = "LatticeNode(%d, %d, %d, %d, %d, %d, %d, %d)";
    tpl.format(startIdx, endIdx, leftId, rightId, genCost, posId, id, totalCost);
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
