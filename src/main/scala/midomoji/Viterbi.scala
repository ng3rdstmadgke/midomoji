package com.github.ng3rdstmadgke.midomoji;

import scala.collection.mutable.HashMap;

case class LatticeNode(val surface: String, val leftId: Int, val rightId: Int, val cost: Int, val pos: Int, val prevIdx: Int) {
  def toDebugString(posInfo: PosInfo): String = {
    val posStr = posInfo.getPos(pos);
    "%s\t%d\t%d\t%d\t%s\t%d".format(surface, leftId, rightId, cost, posStr, prevIdx);
  }
}

class Viterbi(val prefixtree: PrefixTree[Array[Array[Int]]], val matrix: Matrix, val charType: CharType) {
  val tokenizer = new Tokenizer[Array[Array[Int]]](charType, prefixtree);

  def analize(text: String): Option[(List[LatticeNode], Int)] = {
    val lattice = createLattice(text);
    val bos = lattice(0).head;
    val memo = HashMap[LatticeNode, (List[LatticeNode], Int)](bos -> (List(bos), 0));

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
      val eos = lattice(text.length + 1).head;
      Some(go(eos));
    } catch {
      case _ : Throwable => None;
    }
  }

  def createLattice(text: String): Array[List[LatticeNode]] = {
    val len = text.length;
    // Indexは1スタート
    // 辞書からラティス構造を構築
    val lattice = Array.fill[List[LatticeNode]](len + 2)(Nil);
    val bos = LatticeNode("BOS", 0, 0, 0, -1, -1);
    val eos = LatticeNode("EOS", 0, 0, 0, -1, len);
    lattice(0) = List(bos);
    lattice(len + 1) = List(eos);
    (0 until len).foreach { i =>
      val subText  = text.slice(i, len);
      val prevIdx  = i;
      prefixtree.prefixSearch(subText).foreach { elem =>
        val surface = elem._1;
        val tokens  = elem._2;
        val endIdx  = i + surface.length;
        tokens.foreach { token =>
          lattice(endIdx) = LatticeNode(surface, token(0), token(1), token(2), token(3), prevIdx) :: lattice(endIdx);
        }
      }
    }
    // 未知語ノードを追加
    tokenizer.tokenize(text, lattice);
  }
}

object Viterbi {
  def apply(prefixtree: PrefixTree[Array[Array[Int]]], matrix: Matrix, charType: CharType): Viterbi = {
    new Viterbi(prefixtree, matrix, charType);
  }
}
