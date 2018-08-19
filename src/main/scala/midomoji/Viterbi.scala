package com.github.ng3rdstmadgke.midomoji;

import scala.collection.mutable.Queue;
import scala.annotation.tailrec;
import scala.collection.mutable.ArrayBuffer;

class Viterbi(val prefixtree: PrefixTree[Array[Array[Int]]], val matrix: Matrix, val charType: CharType) {
  val tokenizer = new Tokenizer[Array[Array[Int]]](charType, prefixtree);

  def analyze(text: String): Option[LatticeNode] = {
    val (latticeFront, latticeRear) = buildLattice(text);
    val len = text.length;
    val largeCost = 1000000000;
    val eos = latticeFront(len + 1).head;
    val que = Queue[LatticeNode](eos);
    val alreadyAddQue = new Array[Boolean](len + 1);
    @tailrec
    def bfs(): Unit = {
      if (!que.isEmpty) {
        val node = que.dequeue();
        if (node.prevIdx != -1 && !alreadyAddQue(node.prevIdx)) { // bosノードでなければ
          for (prevNode <- latticeRear(node.prevIdx)) {
            que.enqueue(prevNode);
          }
          alreadyAddQue(node.prevIdx) = true;
        }
        if (node.nextIdx != -1) { // eosノードでなければ
          var minCost = largeCost;
          var minNode: LatticeNode = null;
          for (nextNode <- latticeFront(node.nextIdx)) {
            val totalCost = nextNode.totalCost + node.genCost + matrix.getCost(node.rightId, nextNode.leftId);
            if (totalCost < minCost) {
              minCost = totalCost;
              minNode = nextNode;
            }
          }
          node.totalCost = minCost;
          node.nextNode  = minNode;
        }
        bfs();
      }
    }
    bfs();
    val bos = latticeFront(0).head;
    if (bos.nextNode != null) Some(bos) else None;
  }

  def buildLattice(text: String): (Array[ArrayBuffer[LatticeNode]], Array[ArrayBuffer[LatticeNode]]) = {
    val len = text.length;
    // Indexは1スタート
    val latticeFront = Array.fill[ArrayBuffer[LatticeNode]](len + 2)(new ArrayBuffer[LatticeNode]());
    val latticeRear = Array.fill[ArrayBuffer[LatticeNode]](len + 2)(new ArrayBuffer[LatticeNode]());
    val bos   = new LatticeNode("BOS", 0, 0, 0, -1, -1, -1 , 1);
    val eos   = new LatticeNode("EOS", 0, 0, 0, -1, -1, len, -1);
    latticeFront(0)       += bos;
    latticeRear(0)        += bos;
    latticeFront(len + 1) += eos;
    latticeRear(len + 1)  += eos;
    // ---- ---- ---- 辞書の単語を追加 ---- ---- ----
    (0 until len).foreach { i =>
      val subText  = text.slice(i, len);
      val prevLatticeIdx = i;
      prefixtree.prefixSearch(subText).foreach { result =>
        val (surface, tokens) = result;
        val nextLatticeIdx = i + surface.length + 1;
        for (token <- tokens) {
          val Array(leftId, rightId, genCost, posId, id, _*) = token;
          val node = new LatticeNode(surface, leftId, rightId, genCost, posId, id, prevLatticeIdx, nextLatticeIdx);
          latticeFront(prevLatticeIdx + 1) += node;
          latticeRear(nextLatticeIdx - 1)  += node;
        }
      }
    }
    // ---- ---- ---- 未知語を追加 ---- ---- ----
    tokenizer.tokenize(text, latticeFront, latticeRear);
  }
}

object Viterbi {
  def apply(prefixtree: PrefixTree[Array[Array[Int]]], matrix: Matrix, charType: CharType): Viterbi = {
    new Viterbi(prefixtree, matrix, charType);
  }
}
