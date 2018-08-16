package com.github.ng3rdstmadgke.midomoji;

class Viterbi(val prefixtree: PrefixTree[Array[Array[Int]]], val matrix: Matrix, val charType: CharType) {
  val tokenizer = new Tokenizer[Array[Array[Int]]](charType, prefixtree);

  def analyze(text: String): Option[LatticeNode] = {
    val lattice = buildLattice(text);
    val largeCost = 1000000000;
    var reachEos = false;
    val dummy = new LatticeNode("DUMMY", -1, -1, -1, -1, -1, -2);
    def go(node: LatticeNode): Int = {
      if (node.nextIdx == -1) {
        reachEos = true;
        node.totalCost;
      } else if (node.nextNode != null) {
        node.totalCost;
      } else {
        var minCost = largeCost;
        var minNode = dummy;
        for (nextNode <- lattice(node.nextIdx)) {
          val nextTotalCost = go(nextNode);
          val totalCost = nextTotalCost + node.genCost + matrix.getCost(node.rightId, nextNode.leftId);
          if (totalCost < minCost) {
            minCost = totalCost;
            minNode = nextNode;
          }
        }
        node.totalCost = minCost;
        node.nextNode  = minNode;
        minCost;
      }
    }
    val bos = lattice(0).head;
    go(bos);
    if (reachEos) Some(bos) else None;
  }

  def buildLattice(text: String): Array[List[LatticeNode]] = {
    val len = text.length;
    // Indexは1スタート
    val lattice = Array.fill[List[LatticeNode]](len + 2)(Nil);
    val bos   = new LatticeNode("BOS", 0, 0, 0, -1, -1, 1);
    val eos   = new LatticeNode("EOS", 0, 0, 0, -1, -1, -1);
    lattice(0) = List(bos);
    lattice(len + 1) = List(eos);
    // ---- ---- ---- 辞書の単語を追加 ---- ---- ----
    (0 until len).foreach { i =>
      val subText  = text.slice(i, len);
      val latticeIdx = i + 1;
      prefixtree.prefixSearch(subText).foreach { result =>
        val (surface, tokens) = result;
        val nextLatticeIdx = latticeIdx + surface.length;
        lattice(latticeIdx) = tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
          val Array(leftId, rightId, genCost, posId, id) = token;
          new LatticeNode(surface, leftId, rightId, genCost, posId, id, nextLatticeIdx) :: nodes;
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
