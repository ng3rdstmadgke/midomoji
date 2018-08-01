package com.github.ng3rdstmadgke.midomoji;

import scala.collection.mutable.HashMap;

case class LatticeNode(val surface: String, val leftId: Int, val rightId: Int, val cost: Int, val prevIdx: Int);

class Viterbi private (private val prefixtree: PrefixTree[Array[Int]], private val matrix: Matrix) {

  def analize(text: String): Option[(List[LatticeNode], Int)] = {
    val lattice = createLattice(text);
    val bos = LatticeNode("BOS", 0, 0, 0, -1);
    val eos = LatticeNode("EOS", 0, 0, 0, text.length);
    lattice(0) = List(bos);
    val memo = HashMap[LatticeNode, (List[LatticeNode], Int)](bos -> (Nil, 0));

    def go(currNode: LatticeNode): (List[LatticeNode], Int) = {
      memo.get(currNode) match {
        case Some(n) => n;
        case None    => {
          var currNodeList: List[LatticeNode] = null;
          var currCost: Int = Int.MaxValue;
          for (prevNode <- lattice(currNode.prevIdx)) {
            val (prevNodeList, prevCost) = go(prevNode);
            val cost = prevCost + currNode.cost + matrix.getCost(prevNode.rightId, currNode.leftId);
            if (cost < currCost) {
              currCost = cost;
              currNodeList = prevNodeList;
            }
          }
          if (currNodeList == null) {
            throw new RuntimeException("ノードが途中で途切れました");
          } else {
            val ret = (currNode :: currNodeList, currCost);
            memo += (currNode -> ret);
            ret;
          }
        }
      }
    }
    try {
      Some(go(eos));
    } catch {
      case _ : Throwable => None;
    }
  }

  def createLattice(text: String): Array[List[LatticeNode]] = {
    val len = text.length;
    val lattice   = Array.fill[List[LatticeNode]](len + 2)(Nil);
    (0 until len).foreach { i =>
      val subText  = text.slice(i, len);
      val prevIdx  = i;
      prefixtree.prefixSearch(subText).foreach { elem =>
        val surface = elem._1;
        val currIdx  = i + surface.length;
        elem._2.map{ data =>
          LatticeNode(surface, data(0), data(1), data(2), prevIdx);
        }.foreach {
          node => lattice(currIdx) = node :: lattice(currIdx);
        }
      }
    }
    lattice;
  }
}

object Viterbi {
  def apply(ds: DictionarySet[Array[Int]]): Viterbi = {
    new Viterbi(ds.prefixtree, ds.matrix);
  }
  def apply(prefixtree: PrefixTree[Array[Int]], matrix: Matrix): Viterbi = {
    new Viterbi(prefixtree, matrix);
  }
}
