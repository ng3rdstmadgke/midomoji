package com.github.ng3rdstmadgke.midomoji;

class Tokenizer[A](charType: CharType, prefixtree: PrefixTree[A]) {
  def tokenize(text: String, lattice: Array[List[LatticeNode]]): Array[List[LatticeNode]] = {
    val len = text.length;
    val groupTokens = (0 until charType.charTypeNum).toArray.map{ i =>
      GroupToken(0, 0, new StringBuilder(), charType.getTokenConfig(i));
    };
    def go(currIdx: Int): Unit = {
      if (currIdx <= len) {
        val char = text(currIdx - 1);
        val prevIdx = currIdx - 1;
        if (char.isHighSurrogate) {
          // サロゲートペア
          val nextChar = text(currIdx);
          if (nextChar.isLowSurrogate) {
            val str = "" + char + nextChar;
            val charTypeId = 0;
            val tokenConfig = charType.getTokenConfig(charTypeId);
            if (tokenConfig.forceUnigram || prefixtree.find(char) == None) {
              lattice(currIdx + 1) = tokenConfig.tokens.foldLeft(lattice(currIdx + 1)) { (nodes, token) =>
                LatticeNode(str, token.leftId, token.rightId, token.cost, token.pos, -1, -1, prevIdx) :: nodes;
              };
            }
            go(currIdx + 2);
          } else {
            go(currIdx + 1);
          }
        } else if (char.isLowSurrogate) {
          // 不正なサロゲートペア
          go(currIdx + 1);
        } else {
          // 通常の文字
          val str  = char.toString;
          charType.getTokenConfigs(char).foreach { e =>
            val (charTypeId, tokenConfig) = e;
            // unigramトークン生成
            if (tokenConfig.forceUnigram || prefixtree.find(char) == None) {
              lattice(currIdx) = tokenConfig.tokens.foldLeft(lattice(currIdx)) { (nodes, token) =>
                LatticeNode(str, token.leftId, token.rightId, token.cost, token.pos, -1, -1, prevIdx) :: nodes;
              };
            }
            // ngramトークン生成
            if (tokenConfig.ngram > 1) {
              val startIdx = currIdx + 1;
              var endIdx   = currIdx + tokenConfig.ngram;
              endIdx = if (endIdx > len + 1) len + 1 else endIdx;
              var ngram = str;
              (startIdx until endIdx).exists { nextIdx =>
                val nextChar = text(nextIdx - 1);
                if (charType.getCharTypeIds(nextChar).exists(_ == charTypeId)) {
                  ngram += nextChar;
                  lattice(nextIdx) = tokenConfig.tokens.foldLeft(lattice(nextIdx)) { (nodes, token) =>
                    val cost    = if (ngram.length > 3) token.cost * (100 + 10 * ngram.length) / 100 else token.cost;
                    LatticeNode(ngram, token.leftId, token.rightId, cost, token.pos, -1, -1, prevIdx) :: nodes;
                  };
                  false;
                } else {
                  true;
                }
              }
            }
            // groupトークン生成
            if (tokenConfig.groupToken) {
              val groupToken = groupTokens(charTypeId);
              if (groupToken.isEmpty) { // groupToken が空の場合
                groupToken.init(currIdx, char);
              } else {                  // groupToken に作りかけのトークンが存在する場合
                if (groupToken.isNext(currIdx)) {
                  // 同一の文字種が連続していた場合
                  groupToken.add(char);
                } else if (groupToken.shouldCreateToken) {
                  // 直前の文字が異なる文字種でトークンを作る必要がある場合
                  lattice(groupToken.endIdx) = groupToken.tokenConfig.tokens.foldLeft(lattice(groupToken.endIdx)) { (nodes, token) =>
                    val surface = groupToken.surface.toString;
                    val cost    = if (surface.length > 3) token.cost * (100 + 10 * surface.length) / 100 else token.cost;
                    LatticeNode(surface, token.leftId, token.rightId, cost, token.pos, -1, -1, groupToken.startIdx - 1) :: nodes;
                  };
                  groupToken.init(currIdx, char);
                } else {
                  // 直前の文字が異なる文字種でトークンを作る必要がない場合
                  groupToken.init(currIdx, char);
                }
              }
            }
          }
          go(currIdx + 1);
        }
      }
    }
    go(1);
    // 残ったgroupトークンを追加
    groupTokens.foreach { groupToken =>
      if (!groupToken.isEmpty && groupToken.shouldCreateToken) {
        lattice(groupToken.endIdx) = groupToken.tokenConfig.tokens.foldLeft(lattice(groupToken.endIdx)) { (nodes, token) =>
          val surface = groupToken.surface.toString;
          val cost    = if (surface.length > 3) token.cost * (100 + 10 * surface.length) / 100 else token.cost;
          LatticeNode(surface, token.leftId, token.rightId, cost, token.pos, -1, -1, groupToken.startIdx - 1) :: nodes;
        };
      }
    }
    lattice;
  }
}

case class GroupToken(var startIdx: Int, var endIdx: Int, var surface: StringBuilder, val tokenConfig: TokenConfig) {
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
