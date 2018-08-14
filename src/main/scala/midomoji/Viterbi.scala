package com.github.ng3rdstmadgke.midomoji;

class Viterbi(val prefixtree: PrefixTree[Array[Array[Int]]], val matrix: Matrix, val charType: CharType) {
  val tokenizer = new Tokenizer[Array[Array[Int]]](charType, prefixtree);

  def analyze(text: String): Option[LatticeNode] = {
    val lattice = buildLattice(text);
    def go(node: LatticeNode): Int = {
      if (node.prevNode != null || node.prevIdx == -1) {
        node.totalCost;
      } else {
        var minCost = Int.MaxValue;
        var minNode: LatticeNode = null
        for (prevNode <- lattice(node.prevIdx)) {
          val prevTotalCost = go(prevNode);
          val totalCost = prevTotalCost + node.genCost + matrix.getCost(prevNode.rightId, node.leftId);
          if (totalCost < minCost) {
            minCost = totalCost;
            minNode = prevNode;
          }
        }
        node.totalCost = minCost;
        node.prevNode  = minNode;
        minCost;
      }
    }
    try {
      val eos = lattice(text.length + 1).head;
      go(eos);
      Some(eos);
    } catch {
      case _ : Throwable => None;
    }
  }

  def buildLattice(text: String): Array[List[LatticeNode]] = {
    val len = text.length;
    // Indexは1スタート
    val lattice = Array.fill[List[LatticeNode]](len + 2)(Nil);
    val bos   = new LatticeNode("BOS", 0, 0, 0, -1, -1, -1);
    val eos   = new LatticeNode("EOS", 0, 0, 0, -1, -1, len);
    lattice(0) = List(bos);
    lattice(len + 1) = List(eos);
    // ---- ---- ---- 辞書の単語を追加 ---- ---- ----
    (0 until len).foreach { i =>
      val subText  = text.slice(i, len);
      val prevLatticeIdx  = i;
      prefixtree.prefixSearch(subText).foreach { result =>
        val (surface, tokens) = result;
        val latticeIdx = i + surface.length;
        lattice(latticeIdx) = tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
          val Array(leftId, rightId, genCost, posId, id) = token;
          new LatticeNode(surface, leftId, rightId, genCost, posId, id, prevLatticeIdx) :: nodes;
        }
      }
    }
    // ---- ---- ---- 未知語を追加 ---- ---- ----
    tokenizer.tokenize(text, lattice);
  }
}

object Viterbi {
  def apply(prefixtree: PrefixTree[Array[Array[Int]]], matrix: Matrix, charType: CharType): Viterbi = {
    new Viterbi(prefixtree, matrix, charType);
  }
}
