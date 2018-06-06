package com.github.ng3rdstmadgke.midomoji

import org.scalatest._

class PrefixTreeSpec extends FlatSpec with DiagrammedAssertions {

  "PrefixTree add find" should "未登録の要素の取り出し。" in {
    val pt = new PrefixTree[String](1000);
    assert(pt.find("abc") == None);
  }

  "PrefixTree add find" should "未登録の要素の取り出し。配列の長さが足りない場合" in {
    val pt = new PrefixTree[String](10);
    assert(pt.find("abc") == None);
  }

  "PrefixTree add find" should "未登録の要素の取り出し。途中までのキーが登録されている場合" in {
    val pt = new PrefixTree[String](1000);
    pt.add("ab", "ab");
    assert(pt.find("abc") == None);
  }

  "PrefixTree add find" should "未登録の要素の取り出し。遷移は可能だがdataが登録されていない場合" in {
    val pt = new PrefixTree[String](1000);
    pt.add("abcd", "abcd");
    assert(pt.find("abc") == None);
  }

  "PrefixTree add find" should "登録済み要素の取り出し。衝突しない場合" in {
    val pt = new PrefixTree[String](1000);
    pt.add("abc", "abc");
    assert(pt.find("abc") == Some(List("abc")));
  }

  "PrefixTree add find" should "登録済み要素の取り出し。衝突しない・配列の拡張が必要な場合" in {
    val pt = new PrefixTree[String](10);
    pt.add("abc", "abc");
    assert(pt.find("abc") == Some(List("abc")));
  }

  "PrefixTree add find" should "登録済み要素の取り出し。衝突しない・複数要素を登録する場合" in {
    val pt = new PrefixTree[String](10);
    pt.add("abc", "abc");
    pt.add("ad", "ad");
    assert(pt.find("abc") == Some(List("abc")));
    assert(pt.find("ad") == Some(List("ad")));
  }

  "PrefixTree add find" should "登録済み要素の取り出し。衝突する場合" in {
    val pt = new PrefixTree[String](10);
    pt.add("abc", "abc");
    pt.add("ad", "ad");
    pt.add("ac", "ac");
    pt.add("ab", "ab1");
    pt.add("ab", "ab2");
    assert(pt.find("abc") == Some(List("abc")));
    assert(pt.find("ad") == Some(List("ad")));
    assert(pt.find("ac") == Some(List("ac")));
    assert(pt.find("ab") == Some(List("ab2", "ab1")));
  }

  "PrefixTree add find" should "登録済み要素の取り出し。マルチバイト文字" in {
    val pt = new PrefixTree[String](10);
    pt.add("おはよう", "おはよう");
    pt.add("およごう", "およごう");
    assert(pt.find("おはよう") == Some(List("おはよう")));
    assert(pt.find("およごう") == Some(List("およごう")));
  }

  "PrefixTree add find" should "登録済み要素の取り出し。サロゲートペア" in {
    val pt = new PrefixTree[String](10);
    pt.add("𩸽", "𩸽");
    assert(pt.find("𩸽") == Some(List("𩸽")));
  }

  "PrefixTree prefixSearch" should "前方一致検索。" in {
    val pt = new PrefixTree[String](10);
    pt.add("abc", "abc");
    pt.add("ad", "ad");
    pt.add("ac", "ac");
    pt.add("a", "a1");
    pt.add("a", "a2");
    val ret = pt.prefixSearch("abcd").toList;
    assert(ret == List(("a",List("a2", "a1")), ("abc",List("abc"))));
  }
}
