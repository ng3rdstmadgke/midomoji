package com.github.ng3rdstmadgke.midomoji;

/**
 * ラティス構造を構築し、解析するクラス
 *
 * @param prefixtree トライ木オブジェクト
 * @param matrix     連接コスト表オブジェクト
 * @param charType   文字種設定オブジェクト
 */
class Viterbi(private[this] val prefixtree: PrefixTree[Array[Long]],
              private[this] val matrix: Matrix,
              private[this] val charType: CharType,
              private[this] val userPrefixtree: LegacyPrefixTree[Long]) {

  /**
   * ラティス構造を解析するメソッド
   *
   * @param text    解析対象の文字列
   * @return 解析後のラティス構造のBOSノード
   */
  def analyze(text: String): LatticeNode = {
    val lattice = buildLattice(text);
    val sMatrix = matrix;
    var i = text.length;
    while(i >= 0) {
      for (curr <- lattice(i)) {
        val nextIdx = curr.endIdx + 1;
        val genCost = curr.genCost;
        val rightId = curr.rightId;
        var minCost = 1000000000;
        var minNode: LatticeNode = null;
        for (next <- lattice(nextIdx)) {
          val totalCost = next.totalCost + genCost + sMatrix.getCost(rightId, next.leftId);
          if (totalCost < minCost) {
            minCost = totalCost;
            minNode = next;
          }
        }
        curr.totalCost = minCost;
        curr.nextNode  = minNode;
      }
      i -= 1;
    }
    lattice(0).head; // BOS
  }

  /**
   * トライ木に登録されているトークンをラティス構造に追加する
   *
   * @param text    解析対象の文字列
   * @param offset  textのseek開始インデックス
   * @param len     textの長さ
   * @param lattice ラティス構造
   * @param size    トライ木の配列サイズ
   * @param base    トライ木のbase
   * @param check   トライ木のcheck
   * @param data    トライ木のdata
   */
  def addDictToken(text: String, offset: Int     , len: Int         , lattice: Array[List[LatticeNode]],
                   size: Int   , base: Array[Int], check: Array[Int], data: Array[Array[Long]]): Unit = {
    val latticeIdx = offset + 1;
    var currIdx = 1;
    var seek = offset;
    while (seek < len) {
      val nextIdx = base(currIdx) + text(seek);
      if (nextIdx < size && currIdx == check(nextIdx)) {
        if (data(nextIdx) != null) {
          val endIdx         = seek + 1;
          lattice(latticeIdx) = data(nextIdx).foldLeft(lattice(latticeIdx)) { (nodes, token) =>
            new LatticeNode(offset, endIdx, Morpheme.connId(token), Morpheme.connId(token), Morpheme.genCost(token), Morpheme.posId(token), Morpheme.id(token)) :: nodes;
          }
        }
        currIdx = nextIdx;
        seek += 1;
      } else {
        return ();
      }
    }
  }

  /**
   * トライ木に登録されているトークンをラティス構造に追加する
   *
   * @param text     解析対象の文字列
   * @param offset   textのseek開始インデックス
   * @param len      textの長さ
   * @param lattice  ラティス構造
   */
  def addUserDictToken(text: String, offset: Int, len: Int, lattice: Array[List[LatticeNode]]): Unit = {
    var tree = userPrefixtree;
    val latticeIdx = offset + 1;
    var currIdx = 1;
    var seek = offset;
    while (seek < len) {
      val treeNode = tree.get(text(seek));
      if (treeNode == null) {
        return ();
      } else {
       if (treeNode.tree.getData != null) {
         val endIdx = seek + 1;
         lattice(latticeIdx) = treeNode.tree.getData.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
           new LatticeNode(offset, endIdx, Morpheme.connId(token), Morpheme.connId(token), Morpheme.genCost(token), Morpheme.posId(token), Morpheme.id(token)) :: nodes;
         }
       }
      }
      tree = treeNode.tree;
      seek += 1;
    }
  }

  /**
   * ラティス構造を構築するメソッド
   *
   * @param text    解析対象の文字列
   * @return ラティス構造オブジェクト
   */
  def buildLattice(text: String): Array[List[LatticeNode]] = {
    val len = text.length;
    val (size, base, check, data) = prefixtree.getFields;
    // Indexは1スタート
    val lattice = Array.fill[List[LatticeNode]](len + 2)(Nil);
    // startIdx, endIdx, leftId, rightId, genCost, posId, id
    lattice(0)       = new LatticeNode(-1 , 0      , 0, 0, 0, -1, -1) :: Nil;
    lattice(len + 1) = new LatticeNode(len, len + 1, 0, 0, 0, -1, -1) :: Nil;
    val groupTokens = (0 until charType.charTypeNum).toArray.map{ i => new GroupToken(-1, -1, charType.getTokenConfig(i)); }
    var i = 0;
    while (i < len) {
      // ==== ==== ==== 辞書の単語を追加 ==== ==== ====
      addDictToken(text, i, len, lattice, size, base, check, data);
      // ==== ==== ==== ユーザー辞書の単語を追加 ==== ==== ====
      addUserDictToken(text, i, len, lattice);
      // ==== ==== ==== 未知語を追加 ==== ==== ====
      val char = text(i);
      val latticeIdx = i + 1;
      if (char.isSurrogate) {
        // ==== サロゲートペア文字 ====
        val nextIdx = i + 1;
        if (char.isHighSurrogate && nextIdx < len && text(nextIdx).isLowSurrogate) {
          val endIdx = i + 2;
          // 現状サロゲートペアの文字種はからなずDEFAULT
          val TokenConfig(charTypeId, charTypeName, forceUnigram, groupToken, ngram, tokens) = charType.getTokenConfig(0);
          if (forceUnigram || !prefixtree.exists(char)) {
            lattice(latticeIdx) = tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
              val Array(leftId, rightId, genCost, posId, id, _*) = token;
              new LatticeNode(i, endIdx, leftId, rightId, genCost, posId, id) :: nodes;
            }
          }
          i += 2;
        } else {
          i += 1;
        }
      } else {
        // ==== 普通の2byte文字 ====
        charType.getTokenConfigs(char).foreach { tokenConfig =>
          val TokenConfig(charTypeId, charTypeName, forceUnigram, groupToken, ngram, tokens) = tokenConfig;
          // ---- ---- unigramトークン生成 ---- ----
          if (forceUnigram || !prefixtree.exists(char)) {
            val endIdx = i + 1;
            lattice(latticeIdx) = tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
              val Array(leftId, rightId, genCost, posId, id, _*) = token;
              new LatticeNode(i, endIdx, leftId, rightId, genCost, posId, id) :: nodes;
            }
          }
          // ---- ---- ngramトークン生成 ---- ----
          if (ngram > 1) {
            val startIdx = i + 1;
            val endIdx   = if (i + ngram > len) len else i + ngram;
            (startIdx until endIdx).exists { nextIdx =>
              val nextChar   = text(nextIdx);
              val endIdx = nextIdx + 1;
              val surfaceLen = endIdx - i;
              if (charType.typeIs(nextChar, charTypeId) && !prefixtree.exists(text, i, endIdx)) {
                lattice(latticeIdx) = tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
                  val Array(leftId, rightId, genCost, posId, id, _*) = token;
                  val optimizedGenCost = if (surfaceLen > 3) genCost * (100 + 10 * surfaceLen) / 100 else genCost;
                  new LatticeNode(i, endIdx, leftId, rightId, optimizedGenCost, posId, id) :: nodes;
                }
                false;
              } else {
                true;
              }
            }
          }
          // ---- ---- groupトークン生成 ---- ----
          if (groupToken) {
            val gtoken = groupTokens(charTypeId);
            if (gtoken.isEmpty) { // groupToken が空の場合
              gtoken.init(i);
            } else {              // groupToken に作りかけのトークンが存在する場合
              if (gtoken.isNext(i)) {
                // 同一の文字種が連続していた場合
                gtoken.add();
              } else if (gtoken.shouldCreateToken) {
                // 直前の文字が異なる文字種でトークンを作る必要がある場合
                val latticeIdx     = gtoken.startIdx + 1;
                lattice(latticeIdx) = gtoken.tokenConfig.tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
                  val Array(leftId, rightId, genCost, posId, id, _*) = token;
                  val optimizedGenCost = if (gtoken.length > 3) genCost * (100 + 10 * gtoken.length) / 100 else genCost;
                  new LatticeNode(gtoken.startIdx, gtoken.endIdx, leftId, rightId, optimizedGenCost, posId, id) :: nodes;
                }
                gtoken.init(i);
              } else {
                // 直前の文字が異なる文字種でトークンを作る必要がない場合
                gtoken.init(i);
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
        lattice(latticeIdx) = gtoken.tokenConfig.tokens.foldLeft(lattice(latticeIdx)) { (nodes, token) =>
          val Array(leftId, rightId, genCost, posId, id, _*) = token;
          val optimizedGenCost = if (gtoken.length > 3) genCost * (100 + 10 * gtoken.length) / 100 else genCost;
          new LatticeNode(gtoken.startIdx, gtoken.endIdx, leftId, rightId, optimizedGenCost, posId, id) :: nodes;
        }
      }
    }
    lattice;
  }
}

class GroupToken(var startIdx: Int, var endIdx: Int, val tokenConfig: TokenConfig) {
  def length(): Int = endIdx - startIdx;
  def isEmpty: Boolean = length == 0;
  def isNext(idx: Int): Boolean = endIdx == idx;
  def shouldCreateToken: Boolean =  tokenConfig.groupToken && length > 1 && length > tokenConfig.ngram;

  def init(currIdx: Int): Unit = {
    startIdx = currIdx;
    endIdx   = currIdx + 1;
  }

  def add(): Unit = {
    endIdx += 1;
  }
}
