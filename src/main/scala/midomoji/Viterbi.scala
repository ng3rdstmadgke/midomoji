package com.github.ng3rdstmadgke.midomoji;

class Viterbi(val prefixtree: PrefixTree[Array[Array[Int]]], val matrix: Matrix, val charType: CharType) {

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
          val Array(leftId, rightId, genCost, posId, id, _*) = token;
          new LatticeNode(surface, leftId, rightId, genCost, posId, id, nextLatticeIdx) :: nodes;
        }
      }
    }
    // ---- ---- ---- 未知語を追加 ---- ---- ----
    val groupTokens = (0 until charType.charTypeNum).toArray.map{ i =>
      new GroupToken(-1, -1, new StringBuilder(), charType.getTokenConfig(i));
    }
    var i = 0;
    while (i < len) {
      val char = text(i);
      val latticeIdx = i + 1;
      if (char.isSurrogate) {
        val nextIdx = i + 1;
        if (char.isHighSurrogate && nextIdx < len && text(nextIdx).isLowSurrogate) {
          val surface = "" + char + text(nextIdx);
          val nextLatticeIdx = i + 3;
          // 現状サロゲートペアの文字種はからなずDEFAULT
          val TokenConfig(charTypeId, charTypeName, forceUnigram, groupToken, ngram, tokens) = charType.getTokenConfig(0);
          if (forceUnigram || !prefixtree.exists(char)) {
            lattice(latticeIdx) = tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
              val Array(leftId, rightId, genCost, posId, id, _*) = token;
              new LatticeNode(surface, leftId, rightId, genCost, posId, id, nextLatticeIdx) :: nodes;
            }
          }
          i += 2;
        } else {
          i += 1;
        }
      } else {
        // === 普通の2byte文字 ===
        val str = char.toString;
        charType.getTokenConfigs(char).foreach { tokenConfig =>
          val TokenConfig(charTypeId, charTypeName, forceUnigram, groupToken, ngram, tokens) = tokenConfig;
          // unigramトークン生成
          if (forceUnigram || !prefixtree.exists(char)) {
            val surface    = str;
            val nextLatticeIdx = i + 2;
            lattice(latticeIdx) = tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
              val Array(leftId, rightId, genCost, posId, id, _*) = token;
              new LatticeNode(str, leftId, rightId, genCost, posId, id, nextLatticeIdx) :: nodes;
            }
          }
          // ngramトークン生成
          if (ngram > 1) {
            val startIdx = i + 1;
            val endIdx   = if (i + ngram > len) len else i + ngram;
            var surface  = str;
            (startIdx until endIdx).exists { nextIdx =>
              val nextChar   = text(nextIdx);
              val nextLatticeIdx = nextIdx + 2;
              surface += nextChar;
              if (charType.typeIs(nextChar, charTypeId) && !prefixtree.exists(surface)) {
                lattice(latticeIdx) = tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
                  val Array(leftId, rightId, genCost, posId, id, _*) = token;
                  val optimizedGenCost = if (surface.length > 3) genCost * (100 + 10 * surface.length) / 100 else genCost;
                  new LatticeNode(surface, leftId, rightId, optimizedGenCost, posId, id, nextLatticeIdx) :: nodes;
                }
                false;
              } else {
                true;
              }
            }
          }
          // groupトークン生成
          if (groupToken) {
            val gtoken = groupTokens(charTypeId);
            if (gtoken.isEmpty) { // groupToken が空の場合
              gtoken.init(i, char);
            } else {                  // groupToken に作りかけのトークンが存在する場合
              if (gtoken.isNext(i)) {
                // 同一の文字種が連続していた場合
                gtoken.add(char);
              } else if (gtoken.shouldCreateToken) {
                // 直前の文字が異なる文字種でトークンを作る必要がある場合
                val latticeIdx     = gtoken.startIdx + 1;
                val nextLatticeIdx = gtoken.endIdx + 2;
                lattice(latticeIdx) = gtoken.tokenConfig.tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
                  val surface = gtoken.surface.toString;
                  val Array(leftId, rightId, genCost, posId, id, _*) = token;
                  val optimizedGenCost = if (surface.length > 3) genCost * (100 + 10 * surface.length) / 100 else genCost;
                  new LatticeNode(surface, leftId, rightId, optimizedGenCost, posId, id, nextLatticeIdx) :: nodes;
                }
                gtoken.init(i, char);
              } else {
                // 直前の文字が異なる文字種でトークンを作る必要がない場合
                gtoken.init(i, char);
              }
            }
          }
        }
        i += 1;
      }
    }
    // 残ったgroupトークンを追加
    groupTokens.foreach { gtoken =>
      if (!gtoken.isEmpty && gtoken.shouldCreateToken) {
        val latticeIdx     = gtoken.startIdx + 1;
        val nextLatticeIdx = gtoken.endIdx + 2;
        lattice(latticeIdx) = gtoken.tokenConfig.tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
          val surface = gtoken.surface.toString;
          val Array(leftId, rightId, genCost, posId, id, _*) = token;
          val optimizedGenCost = if (surface.length > 3) genCost * (100 + 10 * surface.length) / 100 else genCost;
          new LatticeNode(surface, leftId, rightId, optimizedGenCost, posId, id, nextLatticeIdx) :: nodes;
        }
      }
    }
    lattice;
  }
}

object Viterbi {
  def apply(prefixtree: PrefixTree[Array[Array[Int]]], matrix: Matrix, charType: CharType): Viterbi = {
    new Viterbi(prefixtree, matrix, charType);
  }
}

class GroupToken(var startIdx: Int, var endIdx: Int, var surface: StringBuilder, val tokenConfig: TokenConfig) {
  def isEmpty: Boolean = startIdx == -1 && endIdx == -1;
  def isNext(idx: Int): Boolean = endIdx + 1 == idx;
  def shouldCreateToken: Boolean = {
    val len = surface.length;
    tokenConfig.groupToken && len > 1 && len > tokenConfig.ngram;
  }

  def init(currIdx: Int, char: Char): Unit = {
    startIdx = currIdx;
    endIdx   = currIdx;
    surface.setLength(0);
    surface.append(char);
  }

  def add(char: Char): Unit = {
    endIdx += 1;
    surface.append(char);
  }
}
