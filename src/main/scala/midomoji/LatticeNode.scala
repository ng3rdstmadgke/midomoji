package com.github.ng3rdstmadgke.midomoji;

class LatticeNode(val surface: String, val leftId: Int, val rightId: Int,
                  val genCost: Int   , val posId: Int , val id: Int     , val prevIdx: Int,
                  var totalCost: Int = 0, var prevNode: LatticeNode = null) {
  def foreach(f: LatticeNode => Unit): Unit = {
    this.toList.foreach(node => f(node));
  }
   def toList(): List[LatticeNode] = {
    def go(node: LatticeNode, stack: List[LatticeNode]): List[LatticeNode] = {
      if (node == null) stack else go(node.prevNode, node :: stack);
    }
    go(this, Nil);
  }
   override def toString(): String = {
    val tpl = "LatticeNode(%s, %d, %d, %d, %d, %d, %d)";
    tpl.format(surface, leftId, rightId, genCost, posId, id, totalCost);
  }
}
