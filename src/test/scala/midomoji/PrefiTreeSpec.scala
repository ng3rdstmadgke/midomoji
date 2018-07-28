package com.github.ng3rdstmadgke.midomoji;

import org.scalatest.{FlatSpec, DiagrammedAssertions};
import java.nio.file.{Paths, Files};

class PrefixTreeSpec extends FlatSpec with DiagrammedAssertions {

  "PrefixTree add find" should "未登録の要素の取り出し。" in {
    val pt = PrefixTree[String](1000);
    assert(pt.find("abc") == None);
  }

  it should "未登録の要素の取り出し。配列の長さが足りない場合" in {
    val pt = PrefixTree[String](10);
    assert(pt.find("abc") == None);
  }

  it should "未登録の要素の取り出し。途中までのキーが登録されている場合" in {
    val pt = PrefixTree[String](1000);
    pt.add("ab", "ab");
    assert(pt.find("abc") == None);
  }

  it should "未登録の要素の取り出し。遷移は可能だがdataが登録されていない場合" in {
    val pt = PrefixTree[String](1000);
    pt.add("abcd", "abcd");
    assert(pt.find("abc") == None);
  }

  it should "登録済み要素の取り出し。衝突しない" in {
    val pt = PrefixTree[String](10);
    pt.add("abc", "abc");
    pt.add("ad", "ad");
    assert(pt.find("abc") == Some(List("abc")));
    assert(pt.find("ad") == Some(List("ad")));
  }

  it should "登録済み要素の取り出し。重複した値は登録されない" in {
    val pt = PrefixTree[String](10);
    pt.add("ab", "ab");
    pt.add("ab", "ab");
    assert(pt.find("ab") == Some(List("ab")));
  }

  it should "登録済み要素の取り出し。重複していない値の登録" in {
    val pt = PrefixTree[String](10);
    pt.add("ab", "ab1");
    pt.add("ab", "ab2");
    assert(pt.find("ab") == Some(List("ab2", "ab1")));
  }

  it should "登録済み要素の取り出し。衝突する場合" in {
    val pt = PrefixTree[String](10);
    pt.add("abc", "abc");
    pt.add("ad", "ad");
    pt.add("ac", "ac");
    assert(pt.find("abc") == Some(List("abc")));
    assert(pt.find("ad") == Some(List("ad")));
    assert(pt.find("ac") == Some(List("ac")));
  }

  it should "登録済み要素の取り出し。マルチバイト文字" in {
    val pt = PrefixTree[String](10);
    pt.add("おはよう", "おはよう");
    pt.add("およごう", "およごう");
    assert(pt.find("おはよう") == Some(List("おはよう")));
    assert(pt.find("およごう") == Some(List("およごう")));
  }

  "PrefixTree prefixSearch" should "前方一致検索。" in {
    val pt = PrefixTree[String](10);
    pt.add("abc", "abc");
    pt.add("ad", "ad");
    pt.add("ac", "ac");
    pt.add("a", "a1");
    pt.add("a", "a2");
    val ret = pt.prefixSearch("abcd").toList;
    assert(ret == List(("a",List("a2", "a1")), ("abc",List("abc"))));
  }

/*
  "PrefixTree serialize" should "シリアライズ・デシリアライズ" in {
    val pt = PrefixTree[String](10);
    pt.add("abc", "abc");
    pt.add("ad", "ad");
    pt.add("ac", "ac");
    val dictPath = "dictionary/test_prefixtree.bin";
    pt.serialize(dictPath);
    val pt2 = PrefixTree[String](dictPath);
    assert(pt2.find("abc") == Some(List("abc")));
    assert(pt2.find("ad") == Some(List("ad")));
    assert(pt2.find("ac") == Some(List("ac")));
    Files.deleteIfExists(Paths.get(dictPath));
  }
*/
}
