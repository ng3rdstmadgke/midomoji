package com.github.ng3rdstmadgke.midomoji;

class Tokenizer[A](charType: CharType, prefixtree: PrefixTree[A]) {
  def tokenize(text: String, lattice: Array[List[LatticeNode]]): Array[List[LatticeNode]] = {
    val len = text.length;
    val groupTokens = (0 until charType.charTypeNum).toArray.map{ i =>
      new GroupToken(0, 0, new StringBuilder(), charType.getTokenConfig(i));
    };
    def go(i: Int): Unit = {
      if (i < len) {
        val char = text(i);
        val prevLatticeIdx = i;
        if (char.isHighSurrogate) {
          // === サロゲートペア ===
          val nextIdx = i + 1;
          if (nextIdx < len && text(nextIdx).isLowSurrogate) {
            val surface = "" + char + text(nextIdx);
            val latticeIdx = i + 2;
            // FIXME: CharType 0 が default だということに依存している
            val TokenConfig(charTypeName, forceUnigram, groupToken, ngram, tokens) = charType.getTokenConfig(0);
            if (forceUnigram || prefixtree.find(char) == None) {
              lattice(latticeIdx) = tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
                val Token(leftId, rightId, genCost, posId) = token;
                new LatticeNode(surface, leftId, rightId, genCost, posId, -1, prevLatticeIdx) :: nodes;
              }
            }
            go(i + 2);
          } else {
            go(i + 1);
          }
        } else if (char.isLowSurrogate) {
          // === 不正なサロゲートペア ===
          go(i + 1);
        } else {
          // === 普通の2byte文字 ===
          val str = char.toString;
          charType.getTokenConfigs(char).foreach { config =>
            val (charTypeId, tokenConfig) = config;
            val TokenConfig(charTypeName, forceUnigram, groupToken, ngram, tokens) = tokenConfig;
            // unigramトークン生成
            if (forceUnigram || prefixtree.find(char) == None) {
              val surface    = str;
              val latticeIdx = i + 1;
              lattice(latticeIdx) = tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
                val Token(leftId, rightId, genCost, posId) = token;
                new LatticeNode(str, leftId, rightId, genCost, posId, -1, prevLatticeIdx) :: nodes;
              }
            }
            // ngramトークン生成
            if (ngram > 1) {
              val startIdx = i + 1;
              val endIdx   = if (i + ngram > len) len else i + ngram;
              var surface  = str;
              (startIdx until endIdx).exists { nextIdx =>
                val nextChar   = text(nextIdx);
                val latticeIdx = nextIdx + 1;
                if (charType.typeIs(nextChar, charTypeId)) {
                  surface += nextChar;
                  lattice(latticeIdx) = tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
                    val Token(leftId, rightId, genCost, posId) = token;
                    val optimizedGenCost = if (surface.length > 3) genCost * (100 + 10 * surface.length) / 100 else genCost;
                    new LatticeNode(surface, leftId, rightId, optimizedGenCost, posId, -1, prevLatticeIdx) :: nodes;
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
                  val latticeIdx = gtoken.endIdx + 1;
                  lattice(latticeIdx) = gtoken.tokenConfig.tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
                    val surface = gtoken.surface.toString;
                    val Token(leftId, rightId, genCost, posId) = token;
                    val optimizedGenCost = if (surface.length > 3) genCost * (100 + 10 * surface.length) / 100 else genCost;
                    new LatticeNode(surface, leftId, rightId, optimizedGenCost, posId, -1, gtoken.startIdx) :: nodes;
                  }
                  gtoken.init(i, char);
                } else {
                  // 直前の文字が異なる文字種でトークンを作る必要がない場合
                  gtoken.init(i, char);
                }
              }
            }
          }
          go(i + 1);
        }
      }
    }
    go(0);
    // 残ったgroupトークンを追加
    groupTokens.foreach { gtoken =>
      if (!gtoken.isEmpty && gtoken.shouldCreateToken) {
        val latticeIdx = gtoken.endIdx + 1;
        lattice(latticeIdx) = gtoken.tokenConfig.tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
          val surface = gtoken.surface.toString;
          val Token(leftId, rightId, genCost, posId) = token;
          val optimizedGenCost = if (surface.length > 3) genCost * (100 + 10 * surface.length) / 100 else genCost;
          new LatticeNode(surface, leftId, rightId, optimizedGenCost, posId, -1, gtoken.startIdx) :: nodes;
        }
      }
    }
    lattice;
  }
}

class GroupToken(var startIdx: Int, var endIdx: Int, var surface: StringBuilder, val tokenConfig: TokenConfig) {
  def isEmpty: Boolean = startIdx == 0 && endIdx == 0;
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
