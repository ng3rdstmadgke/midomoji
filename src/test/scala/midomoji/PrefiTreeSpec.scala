package com.github.ng3rdstmadgke.midomoji;

import org.scalatest.{FlatSpec, DiagrammedAssertions};
import java.nio.file.{Paths, Files};

class PrefixTreeSpec extends FlatSpec with DiagrammedAssertions {

  "PrefixTree add find" should "未登録の要素を取り出そうとするとNoneを返す" in {
    val pt = PrefixTree[String](1000);
    assert(pt.find("abc") == None);
  }

  it should "配列の長さが足りない場合はNoneを返す" in {
    val pt = PrefixTree[String](10);
    assert(pt.find("abc") == None);
  }

  it should "途中までのキーが登録されている場合はNoneを返す" in {
    val pt = PrefixTree[String](1000);
    pt.add("ab", "ab");
    assert(pt.find("abc") == None);
  }

  it should "遷移は可能だがdataが登録されていない場合はNoneを返す" in {
    val pt = PrefixTree[String](1000);
    pt.add("abcd", "abcd");
    assert(pt.find("abc") == None);
  }

  it should "衝突しない要素の登録" in {
    val pt = PrefixTree[String](10);
    pt.add("abc", "abc");
    pt.add("ad", "ad");
    assert(pt.find("abc") == Some(List("abc")));
    assert(pt.find("ad") == Some(List("ad")));
  }

  it should "重複した値は登録されない" in {
    val pt = PrefixTree[String](10);
    pt.add("ab", "ab");
    pt.add("ab", "ab");
    assert(pt.find("ab") == Some(List("ab")));
  }

  it should "重複していない値の登録" in {
    val pt = PrefixTree[String](10);
    pt.add("ab", "ab1");
    pt.add("ab", "ab2");
    assert(pt.find("ab") == Some(List("ab2", "ab1")));
  }

  it should "衝突する場合" in {
    val pt = PrefixTree[String](10);
    pt.add("abc", "abc");
    pt.add("ad", "ad");
    pt.add("ac", "ac");
    assert(pt.find("abc") == Some(List("abc")));
    assert(pt.find("ad") == Some(List("ad")));
    assert(pt.find("ac") == Some(List("ac")));
  }

  it should "マルチバイト文字" in {
    val pt = PrefixTree[String](10);
    pt.add("おはよう", "おはよう");
    pt.add("およごう", "およごう");
    assert(pt.find("おはよう") == Some(List("おはよう")));
    assert(pt.find("およごう") == Some(List("およごう")));
  }

  it should "遷移先ノードを正確に取得できているか" in {
    val pt = PrefixTree[String](10);
    pt.add("ba", "ba");
    pt.add("bb", "bb");
    assert(pt.find("ba") == Some(List("ba")));
    assert(pt.find("bb") == Some(List("bb")));
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

}
