package com.github.ng3rdstmadgke.midomoji

import java.io.Closeable;
import scala.util.Try;
import scala.io.Source;
import java.text.Normalizer;

/**
 * 使い終わったリソースを自動でcloseする
 */
object Using {
  def apply[A <: Closeable , B](resource: A)(f: A => B): B = {
    try {
      f(resource);
    } finally {
      resource.close;
    }
  }
}

object Util {
  def toIntOption(s: String): Option[Int] = Try(s.toInt).toOption;

  def parseMTFirst(input: String): (Int, Int) = {
    val list  = input.split(" ");
    if (list.length == 2) {
      val left  = Util.toIntOption(list(0));
      val right = Util.toIntOption(list(1));
      if (left != None && right != None) {
        return (left.get, right.get);
      }
    }
    return (0, 0);
  }

  def parseMT(input: String): Option[(Int, Int, Int)] = {
    val list  = input.split(" ");
    if (list.length == 3) {
      val left  = Util.toIntOption(list(0));
      val right = Util.toIntOption(list(1));
      val cost  = Util.toIntOption(list(2));
      if (left != None && right != None && cost != None) {
        return Some((left.get, right.get, cost.get));
      }
    }
    return None;
  }

  def createMT(path: String): Matrix = {
    Using[Source, Matrix](Source.fromFile(path)) { s =>
      // 1行目はコスト表の縦と横の大きさがスペース区切りで入っている。
      val iter = s.getLines;
      val (leftSize, rightSize) = if (iter.hasNext) Util.parseMTFirst(iter.next()) else (0, 0);
      val mt = Matrix(leftSize, rightSize);
      iter.foreach { line =>
        Util.parseMT(line) match {
          case None            => ();
          case Some((l, r, c)) => mt.setCost(l, r, c);
        }
      }
      mt;
    }
  }

  def checkMT(matrix: Matrix, mtPath: String): Unit = {
    val checkMatrix: Source => Boolean = s => {
      s.getLines.foldLeft(true) { (ret, line) =>
        Util.parseMT(line) match {
          case None            => ret;
          case Some((l, r, c)) => {
            if (c == matrix.getCost(l, r)) {
              ret;
            } else {
              println("left=%d, right=%d, cost=%d, dictCost=%d".format(l, r, c, matrix.getCost(l,r)));
              false;
            }
          }
        }
      }
    }
    // 正しく登録されているかチェック
    if (Using[Source, Boolean](Source.fromFile(mtPath))(checkMatrix)) println("OK!!") else println("NG...");

  }

  def parsePT(input: String): Option[Morpheme] = {
    val list    = input.split(",");
    if (list.length == 13) {
      val surface = Normalizer.normalize(list(0), Normalizer.Form.NFKC);
      val leftId  = Util.toIntOption(list(1));
      val rightId = Util.toIntOption(list(2));
      val cost    = Util.toIntOption(list(3));
      if (leftId != None && rightId != None && cost != None) {
        // 表層形,左文脈ID,右文脈ID,コスト,品詞,品詞細分類1,品詞細分類2,品詞細分類3,活用型,活用形,原形,読み,発音
        val morpheme = Morpheme(surface, leftId.get, rightId.get, cost.get, list(4), list(10), list(11));
        return Some(morpheme);
      }
    }
    return None;
  }

  def createPT(path: String): PrefixTree[Morpheme] = {
    Using[Source, PrefixTree[Morpheme]](Source.fromFile(path)) { s =>
      val pt = PrefixTree[Morpheme](500000);
      s.getLines.foreach { line =>
        Util.parsePT(line) match {
          case None           => ();
          case Some(morpheme) => pt.add(morpheme.surface, morpheme);
        }
      }
      pt;
    }
  }

  def checkPT(prefixree: PrefixTree[Morpheme], ptPath: String): Unit = {
    val checkPT: Source => Boolean = s => {
      // 表層形,左文脈ID,右文脈ID,コスト,品詞,品詞細分類1,品詞細分類2,品詞細分類3,活用型,活用形,原形,読み,発音
      var cnt = 0;
      var ret = s.getLines.foldLeft(true) { (ret, line) =>
        Util.parsePT(line) match {
          case None           => ret;
          case Some(morpheme) => {
            val morphemes = prefixree.find(morpheme.surface);
            val exists = morphemes match {
              case None     => false;
              case Some(ms) => ms.exists(m => if (morpheme == m) true else false);
            }
            if (exists) {
              ret;
            } else {
              println("input=%s, output=%s".format(morpheme.surface, morphemes));
              cnt += 1;
              false;
            }
          }
        }
      }
      println("failcnt = %d".format(cnt));
      ret;
    }
    // 正しく登録されているかチェック
    if (Using[Source, Boolean](Source.fromFile(ptPath))(checkPT)) println("OK!!") else println("NG...");
  }
}
