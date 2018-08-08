package com.github.ng3rdstmadgke.midomoji;

import scala.collection.mutable.Queue;
import scala.collection.AbstractIterator;

class Tokenizer(charTypeArr: Array[Array[Int]], tokenSettings: Array[TokenSetting], prefixtree: PrefixTree[Array[Array[Int]]]) {
  def tokenize(text: String): TokenIterator = new TokenIterator(text, charTypeArr, tokenSettings, prefixtree);
}
object Tokenizer {
  def apply(prefixtree: PrefixTree[Array[Array[Int]]]): Tokenizer = {
    // 文字種表の作成
    val arr = new Array[List[Int]](Char.MaxValue + 1);
    TokenizerDef.CHAR_TYPE_DEF.zipWithIndex.foreach { pair =>
      val (rangeSet, charType) = pair;
      rangeSet.foreach { range =>
        val (first, last) = range;
        (first to last).foreach { i =>
          arr(i) = if (arr(i) == null) List(charType) else  charType :: arr(i);
        }
      }
    }
    val charTypeArr = arr.map( e => if (e == null) Array(0) else e.toArray);
    // 未知語トークンの生成ルール表の作成
    val tokenSettings = TokenizerDef.UNK_TOKEN_SETTINGS_DEF.map { e =>
      val (invoke, group, length, tokensDef) = e;
      val tokens = tokensDef.map { str =>
        val list = str.split(",");
        Array(list(0).toInt, list(1).toInt, list(2).toInt);
      }
      TokenSetting(invoke, group, length, tokens);
    }
    new Tokenizer(charTypeArr, tokenSettings, prefixtree);
  }
}

class TokenIterator(text: String, charTypeArr: Array[Array[Int]], tokenSettings: Array[TokenSetting], prefixtree: PrefixTree[Array[Array[Int]]]) extends AbstractIterator[(Int, Int, String, Array[Array[Int]])] {
  val len = text.length;
  // 現在のindex。スタートは1
  var currIdx = 1;
  // 作りかけのgroup トークンを保持しておくための配列
  val groupTokens = new Array[GroupToken](TokenizerDef.CHAR_TYPES_NUM).map( e => GroupToken(0, 0, new StringBuilder()));
  // トークンの格納場所
  val que = Queue[(Int, Int, String, Array[Array[Int]])]();

  // (startインデックス, endインデックス, surface, tokens) を返す
  override def next(): (Int, Int, String, Array[Array[Int]]) = que.dequeue;

  override def hasNext: Boolean = {
    if (!que.isEmpty) {
      true;
    } else if (currIdx > len) {
      for (i <- (0 until TokenizerDef.CHAR_TYPES_NUM)) {
        val groupToken = groupTokens(i);
        if (!groupToken.isEmpty) {
          que.enqueue((groupToken.startIdx, groupToken.endIdx, groupToken.surface.toString, tokenSettings(i).tokens));
          groupToken.startIdx = 0;
          groupToken.endIdx   = 0;
          groupToken.surface.setLength(0);
        }
      }
      if (que.isEmpty) false else true;
    } else {
      val char = text(currIdx - 1);
      val charTypes = charTypeArr(char.toInt);
      charTypes.foreach { charType =>
        val setting = tokenSettings(charType);
        // unigram トークン生成
        if (setting.invoke || prefixtree.find(char.toString) == None) {
          que.enqueue((currIdx, currIdx, char.toString, setting.tokens));
        }
        // ngram トークン生成
        if (setting.ngram > 1) {
          val start = currIdx;
          val end   = currIdx + setting.ngram - 1;
          var ngram = char.toString;
          (start until end).exists { i =>
            val nextCharTypes = charTypeArr(text(i).toInt);
            if (nextCharTypes.exists(_ == charType)) {
              ngram += text(i);
              que.enqueue((currIdx, i + 1, ngram, setting.tokens));
              false;
            } else {
              true;
            }
          }
        }
        // group トークン生成
        if (setting.group) {
          val groupToken = groupTokens(charType);
          if (groupToken.isEmpty) {                   // textの先頭もしくは、新しい文字種になったとき
            groupToken.startIdx = currIdx;
            groupToken.endIdx   = currIdx;
            groupToken.surface.append(char);
          } else if (groupToken.endIdx + 1 == currIdx) { // 同一の文字種が連続していた場合
            groupToken.endIdx += 1;
            groupToken.surface.append(char);
          } else {                                    // 直前の文字が異なる文字種だった場合
            que.enqueue((groupToken.startIdx, groupToken.endIdx, groupToken.surface.toString, setting.tokens));
            groupToken.startIdx = 0;
            groupToken.endIdx   = 0;
            groupToken.surface.setLength(0);
          }
        }
      }
      currIdx += 1;
      if (!que.isEmpty) hasNext else true;
    }
  }
}

case class GroupToken(var startIdx: Int, var endIdx: Int, var surface: StringBuilder) {
  def isEmpty: Boolean = startIdx == 0 && endIdx == 0;
}

case class TokenSetting(invoke: Boolean, group: Boolean, ngram: Int, tokens: Array[Array[Int]]);

object TokenizerDef {
  val CHAR_TYPES_NUM    = 11;
  val DEFAULT: Int      = 0;
  val SPACE: Int        = 1;
  val KANJI: Int        = 2;
  val SYMBOL: Int       = 3;
  val NUMERIC: Int      = 4;
  val ALPHA: Int        = 5;
  val HIRAGANA: Int     = 6;
  val KATAKANA: Int     = 7;
  val KANJINUMERIC: Int = 8;
  val GREEK: Int        = 9;
  val CYRILLIC: Int     = 10;
  val UNK_TOKEN_SETTINGS_DEF: Array[(Boolean, Boolean, Int, Array[String])] = Array( // (invoke, group, ngram, tokens)
    // DEFAULT
    (false, true , 0, Array("5,5,4769,記号,一般,*,*,*,*,*")),
    // SPACE
    (false, true , 0, Array("9,9,8903,記号,空白,*,*,*,*,*")),
    // KANJI
    (false, false, 2, Array("1285,1285,11426,名詞,一般,*,*,*,*,*",
                            "1283,1283,17290,名詞,サ変接続,*,*,*,*,*",
                            "1293,1293,17611,名詞,固有名詞,地域,一般,*,*,*",
                            "1292,1292,12649,名詞,固有名詞,組織,*,*,*,*",
                            "1289,1289,17340,名詞,固有名詞,人名,一般,*,*,*",
                            "1288,1288,15295,名詞,固有名詞,一般,*,*,*,*")),
    // SYMBOL
    (true , true , 0, Array("1283,1283,17585,名詞,サ変接続,*,*,*,*,*")),
    // NUMERIC
    (true , true , 0, Array("1295,1295,27386,名詞,数,*,*,*,*,*")),
    // ALPHA
    (true , true , 0, Array("1285,1285,13398,名詞,一般,*,*,*,*,*",
                            "1293,1293,18706,名詞,固有名詞,地域,一般,*,*,*",
                            "1292,1292,13835,名詞,固有名詞,組織,*,*,*,*",
                            "1289,1289,18188,名詞,固有名詞,人名,一般,*,*,*",
                            "1288,1288,15673,名詞,固有名詞,一般,*,*,*,*",
                            "3,3,15235,感動詞,*,*,*,*,*,*")),
    // HIRAGANA
    (false, true , 2, Array("1285,1285,13069,名詞,一般,*,*,*,*,*",
                            "1283,1283,20223,名詞,サ変接続,*,*,*,*,*",
                            "1293,1293,17882,名詞,固有名詞,地域,一般,*,*,*",
                            "1292,1292,14761,名詞,固有名詞,組織,*,*,*,*",
                            "1289,1289,18060,名詞,固有名詞,人名,一般,*,*,*",
                            "1288,1288,14787,名詞,固有名詞,一般,*,*,*,*",
                            "3,3,16989,感動詞,*,*,*,*,*,*")),
    // KATAKANA
    (true , true , 2, Array("1285,1285,9461,名詞,一般,*,*,*,*,*",
                            "1293,1293,13661,名詞,固有名詞,地域,一般,*,*,*",
                            "1292,1292,10922,名詞,固有名詞,組織,*,*,*,*",
                            "1289,1289,13581,名詞,固有名詞,人名,一般,*,*,*",
                            "1288,1288,10521,名詞,固有名詞,一般,*,*,*,*",
                            "3,3,14138,感動詞,*,*,*,*,*,*")),
    // KANJINUMERIC
    (true , true , 0, Array("1295,1295,27473,名詞,数,*,*,*,*,*")),
    // GREEK
    (true , true , 0, Array("1285,1285,7884,名詞,一般,*,*,*,*,*",
                            "1293,1293,12681,名詞,固有名詞,地域,一般,*,*,*",
                            "1292,1292,8573,名詞,固有名詞,組織,*,*,*,*",
                            "1289,1289,12697,名詞,固有名詞,人名,一般,*,*,*",
                            "1288,1288,10029,名詞,固有名詞,一般,*,*,*,*")),
    // CYRILLIC
    (true , true , 0, Array("1285,1285,7966,名詞,一般,*,*,*,*,*",
                            "1293,1293,12600,名詞,固有名詞,地域,一般,*,*,*",
                            "1292,1292,8492,名詞,固有名詞,組織,*,*,*,*",
                            "1289,1289,12615,名詞,固有名詞,人名,一般,*,*,*",
                            "1288,1288,9866,名詞,固有名詞,一般,*,*,*,*")),
  );
  val CHAR_TYPE_DEF: Array[Array[(Int, Int)]] = Array(
    // DEFAULT
    Array(),
    // SPACE
    Array((0x0020, 0x0020), (0x00D0, 0x00D0), (0x0009, 0x0009), (0x000B, 0x000B), (0x000A, 0x000A)),
    // KANJI
    Array((0x2E80, 0x2EF3), (0x2F00, 0x2FD5), (0x3005, 0x3005), (0x3007, 0x3007), (0x3400, 0x4DB5),
          (0x4E00, 0x9FA5), (0xF900, 0xFA2D), (0xFA30, 0xFA6A)),
    // SYMBOL
    Array((0x0021, 0x002F), (0x003A, 0x0040), (0x005B, 0x0060), (0x007B, 0x007E), (0x00A1, 0x00BF),
          (0xFF01, 0xFF0F), (0xFF1A, 0xFF1F), (0xFF3B, 0xFF40), (0xFF5B, 0xFF65), (0xFFE0, 0xFFEF),
          (0x2000, 0x206F), (0x20A0, 0x20CF), (0x20D0, 0x20FF), (0x2100, 0x214F), (0x2100, 0x214B),
          (0x2190, 0x21FF), (0x2200, 0x22FF), (0x2300, 0x23FF), (0x2460, 0x24FF), (0x2501, 0x257F),
          (0x2580, 0x259F), (0x25A0, 0x25FF), (0x2600, 0x26FE), (0x2700, 0x27BF), (0x27F0, 0x27FF),
          (0x27C0, 0x27EF), (0x2800, 0x28FF), (0x2900, 0x297F), (0x2B00, 0x2BFF), (0x2A00, 0x2AFF),
          (0x3300, 0x33FF), (0x3200, 0x32FE), (0x3000, 0x303F), (0xFE30, 0xFE4F), (0xFE50, 0xFE6B),
          (0x3007, 0x3007)),
    // NUMERIC
    Array((0x0030, 0x0039), (0xFF10, 0xFF19), (0x2070, 0x209F), (0x2150, 0x218F)),
    // ALPHA
    Array((0x0041, 0x005A), (0x0061, 0x007A), (0x00C0, 0x00FF), (0x0100, 0x017F),
          (0x0180, 0x0236), (0x1E00, 0x1EF9), (0xFF21, 0xFF3A), (0xFF41, 0xFF5A)),
    // HIRAGANA
    Array((0x3041, 0x309F)),
    // KATAKANA
    Array((0x30A1, 0x30FF), (0x31F0, 0x31FF), (0xFF66, 0xFF9D), (0xFF9E, 0xFF9F)),
    // KANJINUMERIC
    Array((0x4E00, 0x4E00), (0x4E8C, 0x4E8C), (0x4E09, 0x4E09), (0x56DB, 0x56DB), (0x4E94, 0x4E94),
          (0x516D, 0x516D), (0x4E03, 0x4E03), (0x516B, 0x516B), (0x4E5D, 0x4E5D), (0x5341, 0x5341),
          (0x767E, 0x767E), (0x5343, 0x5343), (0x4E07, 0x4E07), (0x5104, 0x5104), (0x5146, 0x5146),
          (0x3007, 0x3007)),
    // GREEK
    Array((0x0374, 0x03FB)),
    // CYRILLIC
    Array((0x0400, 0x04F9), (0x0500, 0x050F))
    );
}
