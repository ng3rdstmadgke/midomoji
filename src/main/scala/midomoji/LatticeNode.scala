package com.github.ng3rdstmadgke.midomoji;

import scala.collection.Iterable;
import scala.collection.AbstractIterator;

class LatticeNode(val surface: String, val leftId: Int, val rightId: Int,
                  val genCost: Int   , val posId: Int , val id: Int     , val nextIdx: Int,
                  var totalCost: Int = 0, var nextNode: LatticeNode = null) extends Iterable[LatticeNode] {
  override def iterator: Iterator[LatticeNode] = new LatticeNodeIterator(this);

  override def toString(): String = {
    val tpl = "LatticeNode(%s, %d, %d, %d, %d, %d, %d)";
    tpl.format(surface, leftId, rightId, genCost, posId, id, totalCost);
  }
}

class LatticeNodeIterator(var node: LatticeNode) extends AbstractIterator[LatticeNode] {
  override def next(): LatticeNode = {
    val ret = node;
    node = node.nextNode;
    ret;
  }
  override def hasNext: Boolean = if (node != null) true else false;
}
