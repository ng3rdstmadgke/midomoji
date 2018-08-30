package com.github.ng3rdstmadgke.midomoji;

import scala.collection.mutable.ArrayBuffer;
import scala.collection.Iterable;
import scala.collection.AbstractIterator;

class LatticeIterator(val lattice: Array[ArrayBuffer[Array[Int]]]) extends AbstractIterator[Array[Int]] {
  val bosNextIdx  = lattice(0)(0)(8);
  var node = lattice(1)(bosNextIdx);

  override def next(): Array[Int] = {
    val ret = node;
    val nextIdx     = node(1) + 1;
    val nextNodeIdx = node(8);
    node = lattice(nextIdx)(nextNodeIdx);
    ret;
  }

  override def hasNext: Boolean = {
    val nextNodeIdx = node(8);
    if (nextNodeIdx < 0) false else true;
  }
}

class Lattice(private[this] val prefixtree: PrefixTree[Array[Array[Int]]],
              private[this] val charType: CharType,
              private[this] val matrix: Matrix) {
  private[this] var lattice = Array[ArrayBuffer[Array[Int]]]();
  private[this] val (treeSize, base, check, data) = prefixtree.getFields;
  private[this] val groupTokens = (0 until charType.charTypeNum).toArray.map{i => new GroupToken(-1, -1, charType.getTokenConfig(i))};

  def getLattice(): Array[ArrayBuffer[Array[Int]]] = lattice;

  def analyze(text: String): LatticeIterator = {
    def go(node: Array[Int], sLattice: Array[ArrayBuffer[Array[Int]]], sMatrix: Matrix): Int = {
      val Array(startIdx, endIdx , leftId, rightId, genCost, posId, id, totalCost, nextNodeIdx) = node;
      if (nextNodeIdx != -1) { // 計算済みもしくはeosノードの場合
        totalCost;
      } else {                        // 未計算ノードの場合
        val nextNodes = sLattice(endIdx + 1);
        val len = nextNodes.length;
        var cnt = 0;
        var minCost = 1000000000;
        var minNodeIdx = -2;
        while (cnt < len) {
          val nextNode = nextNodes(cnt);
          val nextTotalCost = go(nextNode, sLattice, sMatrix);
          val totalCost = nextTotalCost + genCost + sMatrix.getCost(rightId, nextNode(3));
          if (totalCost < minCost) {
            minCost    = totalCost;
            minNodeIdx = cnt;
          }
          cnt += 1;
        }
        node(7) = minCost;
        node(8) = minNodeIdx;
        minCost;
      }
    }
    build(text);
    val bos = lattice(0)(0);
    val eos = lattice(text.length + 1)(0);
    go(bos, lattice, matrix);
    new LatticeIterator(lattice);
  }

  def build(text: String): Unit = {
    val len = text.length;
    lattice = Array.fill[ArrayBuffer[Array[Int]]](len + 2)(ArrayBuffer());
    val sLattice = lattice;
    // nextNodeIdx : -1 = 未計算, -2 = 遷移先ノードがない, -3 = eosノード
    //                   Array(startIdx, endIdx , leftId, rightId, genCost, posId, id, totalCost, nextNodeIdx)
    sLattice(0)       += Array(-1      , 0      , 0     , 0      , 0      , -1   , -1, 0        , -1);
    sLattice(len + 1) += Array(len     , len + 1, 0     , 0      , 0      , -1   , -1, 0        , -3);
    var offset  = 0;
    groupTokens.foreach(_.init());
    while (offset < len) {
      val latticeIdx = offset + 1;
      // ==== ==== ==== 辞書の単語を追加 ==== ==== ====
      addDictToken(text, offset, len, latticeIdx);
      // ==== ==== ==== 未知語を追加 ==== ==== ====
      val char = text(offset);
      if (!char.isSurrogate) {
        // === 通常の2byte文字 ===
        charType.getTokenConfigs(char).foreach { tokenConfig =>
          val TokenConfig(charTypeId, charTypeName, forceUnigram, groupToken, ngram, tokens) = tokenConfig;
          // ---- ---- unigramトークン生成 ---- ----
          addUnigramToken(text, offset, forceUnigram, tokens);
          // ---- ---- ngramトークン生成 ---- ----
          addNgramToken(text, offset, charTypeId, ngram, tokens);
          // ---- ---- groupトークン生成 ---- ----
          addGroupToken(offset, charTypeId, groupToken);
        }
      } else {
        // === サロゲートペア ===
        if (char.isHighSurrogate) {
          val nextIdx = offset + 1;
          if (nextIdx < len && text(nextIdx).isLowSurrogate) {
            val exists = inPrefixtree(text, offset, 2);
            if (!exists) {
              // 現状サロゲートペアの文字種はからなずDEFAULT
              charType.getTokenConfig(0).tokens.foreach { token =>
                // val Array(leftId, rightId, genCost, posId, id, _*) = token;
                sLattice(latticeIdx) += Array(offset, offset + 2, token(0), token(1), token(2), token(3), token(4), 0, -1);
              }
            }
            offset += 1;
          }
        }
      }
      // --------------------
      offset += 1;
    }
    // 残ったgroupトークンを追加
    groupTokens.foreach { gtoken =>
      if (gtoken.shouldCreateToken) {
        val latticeIdx = gtoken.getStartIdx + 1;
        gtoken.gettokenConfig.tokens.foreach { token =>
          val len = gtoken.getEndIdx - gtoken.getStartIdx;
          val genCost = if (len > 3) token(2) * (100 + 10 * len) / 100 else token(2);
          lattice(latticeIdx) += Array(gtoken.getStartIdx, gtoken.getEndIdx, token(0), token(1), genCost, token(3), token(4), 0, -1);
        }
      }
    }
  }

  def addUnigramToken(text: String, offset: Int, forceUnigram: Boolean, tokens: Array[Array[Int]]): Unit = {
    val sLattice = lattice;
    val exists = inPrefixtree(text, offset, offset + 1);
    val latticeIdx = offset + 1;
    if (forceUnigram || !exists) {
      tokens.foreach { token =>
        // val Array(leftId, rightId, genCost, posId, id, _*) = token;
        sLattice(latticeIdx) += Array(offset, offset + 1, token(0), token(1), token(2), token(3), token(4), 0, -1);
      }
    }
  }

  def addNgramToken(text: String, offset: Int, charTypeId: Int, ngram: Int, tokens: Array[Array[Int]]): Unit = {
    val sLattice = lattice;
    val latticeIdx = offset + 1;
    if (ngram > 1) {
      val startIdx = offset + 1;
      val endIdx   = Math.min(text.length, offset + ngram);
      var cnt      = 1;
      (startIdx until endIdx).foreach { nextIdx =>
        val nextChar   = text(nextIdx);
        val exists     = inPrefixtree(text, offset, nextIdx + 1);
        if (charType.typeIs(nextChar, charTypeId) && !exists) {
          tokens.foreach { token =>
            // val Array(leftId, rightId, genCost, posId, id, _*) = token;
            val len = nextIdx - offset + 1;
            val genCost = if (len > 3) token(2) * (100 + 10 * len) / 100 else token(2);
            sLattice(latticeIdx) += Array(offset, nextIdx + 1, token(0), token(1), genCost, token(3), token(4), 0, -1);
          }
        } else {
          return ();
        }
      }
    }
  }

  def addGroupToken(offset: Int, charTypeId: Int, groupToken: Boolean): Unit = {
    val sLattice = lattice;
    if (groupToken) {
      val gtoken = groupTokens(charTypeId);
      if (gtoken.isEmpty) {
      // groupToken が空の場合
        gtoken.addFirst(offset);
      } else {
      // groupToken に作りかけのトークンが存在する場合
        if (gtoken.isNext(offset)) {
          // 同一の文字種が連続していた場合
          gtoken.add();
        } else {
          if (gtoken.shouldCreateToken) {
            // 直前の文字が異なる文字種でトークンを作る必要がある場合
            val latticeIdx     = gtoken.getStartIdx + 1;
            gtoken.gettokenConfig.tokens.foreach { token =>
              val len = gtoken.getEndIdx - gtoken.getStartIdx;
              val genCost = if (len > 3) token(2) * (100 + 10 * len) / 100 else token(2);
              sLattice(latticeIdx) += Array(gtoken.getStartIdx, gtoken.getEndIdx, token(0), token(1), genCost, token(3), token(4), 0, -1);
            }
          }
          gtoken.addFirst(offset);
        }
      }
    }
  }

  def inPrefixtree(text: String, startIdx: Int, endIdx: Int): Boolean = {
    val sBase = base; val sCheck = check; val sData = data;
    var currIdx = 1;
    var seek    = startIdx;
    val end     = Math.min(endIdx, text.length);
    while (seek < end) {
      val nextIdx = sBase(currIdx) + text(seek);
      if (currIdx != sCheck(nextIdx)) {
        return false;
      }
      currIdx = nextIdx;
      seek += 1;
    }
    if (sData(currIdx) == null) false else true;
  }

  def addDictToken(text: String, offset: Int, len: Int, latticeIdx: Int): Unit = {
    val sLattice = lattice; val sBase = base; val sCheck = check; val sData = data;
    var currIdx = 1;
    var seek = offset;
    while (seek < len) {
      val nextIdx = sBase(currIdx) + text(seek);
      if (currIdx == sCheck(nextIdx)) {
        if (sData(nextIdx) != null) {
          sData(nextIdx).foreach { token =>
            // val Array(leftId, rightId, genCost, posId, id, _*) = token;
            sLattice(latticeIdx) += Array(offset, seek + 1, token(0), token(1), token(2), token(3), token(4), 0, -1);
          }
        }
        currIdx = nextIdx;
        seek += 1;
      } else {
        return ();
      }
    }
  }
}

class GroupToken(private[this] var startIdx: Int,
                 private[this] var endIdx: Int,
                 private[this] val tokenConfig: TokenConfig) {
  def getStartIdx(): Int = startIdx;
  def getEndIdx(): Int = endIdx;
  def gettokenConfig(): TokenConfig = tokenConfig;

  def isEmpty: Boolean = startIdx == -1 && endIdx == -1;

  def isNext(idx: Int): Boolean = endIdx == idx;

  def shouldCreateToken: Boolean = {
    val len = endIdx - startIdx;
    tokenConfig.groupToken && len > 1 && len > tokenConfig.ngram;
  }

  def init(): Unit = {
    startIdx = -1;
    endIdx   = -1;
  }

  def add(): Unit = {
    endIdx = endIdx + 1;
  }

  def addFirst(currIdx: Int): Unit = {
    startIdx = currIdx;
    endIdx   = currIdx + 1;
  }
}
