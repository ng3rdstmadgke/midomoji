package com.github.ng3rdstmadgke.midomoji;

import org.scalatest.{FlatSpec, DiagrammedAssertions};
import java.nio.file.{Paths, Files};

class PrefixTreeSpec extends FlatSpec with DiagrammedAssertions {

  def addFunc(existing: List[String], elem: String): List[String] = {
    existing match {
      case ls: List[String] => elem :: existing;
      case _ => List(elem);
    }
  }

  "PrefixTree add find" should "未登録の要素を取り出そうとするとNoneを返す" in {
    val pt = PrefixTree[List[String]](1000);
    assert(pt.find("abc") == None);
  }

  it should "配列の長さが足りない場合はNoneを返す" in {
    val pt = PrefixTree[List[String]](10);
    assert(pt.find("abc") == None);
  }

  it should "途中までのキーが登録されている場合はNoneを返す" in {
    val pt = PrefixTree[List[String]](1000);
    pt.add("ab", "ab")(addFunc);
    assert(pt.find("abc") == None);
  }

  it should "遷移は可能だがdataが登録されていない場合はNoneを返す" in {
    val pt = PrefixTree[List[String]](1000);
    pt.add("abcd", "abcd")(addFunc);
    assert(pt.find("abc") == None);
  }

  it should "衝突しない要素の登録" in {
    val pt = PrefixTree[List[String]](10);
    pt.add("abc", "abc")(addFunc);
    pt.add("ad", "ad")(addFunc);
    assert(pt.find("abc") == Some(List("abc")));
    assert(pt.find("ad") == Some(List("ad")));
  }

  it should "重複していない値の登録" in {
    val pt = PrefixTree[List[String]](10);
    pt.add("ab", "ab1")(addFunc);
    pt.add("ab", "ab2")(addFunc);
    assert(pt.find("ab") == Some(List("ab2", "ab1")));
  }

  it should "衝突する場合" in {
    val pt = PrefixTree[List[String]](10);
    pt.add("abc", "abc")(addFunc);
    pt.add("ad", "ad")(addFunc);
    pt.add("ac", "ac")(addFunc);
    assert(pt.find("abc") == Some(List("abc")));
    assert(pt.find("ad") == Some(List("ad")));
    assert(pt.find("ac") == Some(List("ac")));
  }

  it should "マルチバイト文字" in {
    val pt = PrefixTree[List[String]](10);
    pt.add("おはよう", "おはよう")(addFunc);
    pt.add("およごう", "およごう")(addFunc);
    assert(pt.find("おはよう") == Some(List("おはよう")));
    assert(pt.find("およごう") == Some(List("およごう")));
  }

  it should "遷移先ノードを正確に取得できているか" in {
    val pt = PrefixTree[List[String]](10);
    pt.add("ba", "ba")(addFunc);
    pt.add("bb", "bb")(addFunc);
    assert(pt.find("ba") == Some(List("ba")));
    assert(pt.find("bb") == Some(List("bb")));
  }

  "PrefixTree prefixSearch" should "前方一致検索。" in {
    val pt = PrefixTree[List[String]](10);
    pt.add("abc", "abc")(addFunc);
    pt.add("ad", "ad")(addFunc);
    pt.add("ac", "ac")(addFunc);
    pt.add("a", "a1")(addFunc);
    pt.add("a", "a2")(addFunc);
    val ret = pt.prefixSearch("abcd").toList;
    assert(ret == List(("a",List("a2", "a1")), ("abc",List("abc"))));
  }

}
