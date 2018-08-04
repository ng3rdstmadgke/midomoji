package com.github.ng3rdstmadgke.midomoji

import java.io.Closeable;
import scala.util.Try;
import scala.io.Source;
import java.text.Normalizer;
import scala.collection.mutable.HashMap;

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

  def parsePos(input: String): Option[(String, Int)] = {
    val list = input.split(" ");
    if (list.length == 2) {
      val pos = list(0);
      val id = Util.toIntOption(list(1));
      if (id != None) {
        return Some((pos, id.get));
      }
    }
    return None;
  }

  def createPosMap(path: String): (HashMap[String, Int], Array[Array[String]]) = {
    Using[Source, (HashMap[String, Int], Array[Array[String]])](Source.fromFile(path)) { s =>
      val posMap = HashMap[String, Int]();
      var maxId = 0;
      s.getLines.foreach { line =>
        Util.parsePos(line) match {
          case None          => ();
          case Some((pos, id)) => {
            posMap += (pos -> id);
            maxId = if (id > maxId) id else maxId;
          }
        }
      }
      val posArr = new Array[Array[String]](maxId + 1);
      posMap.iterator.foreach { e =>
        posArr(e._2) = e._1.split(",");
      }
      (posMap, posArr);
    }
  }

  def parsePT(input: String, posMap: HashMap[String, Int]): Option[(String, Array[Int])] = {
    val list    = input.split(",");
    if (list.length == 13) {
      val surface = list(0);
      val leftId  = Util.toIntOption(list(1));
      val rightId = Util.toIntOption(list(2));
      val cost    = Util.toIntOption(list(3));
      val pos     = list(4) + "," + list(5) + "," + list(6) + "," + list(7);
      val posId   = posMap.getOrElse(pos, -1);
      if (leftId != None && rightId != None && cost != None) {
        return Some((surface, Array(leftId.get, rightId.get, cost.get, posId)));
      }
    }
    return None;
  }

  def parsePT2(input: String): Option[(String, Array[Int])] = {
    val list    = input.split(",");
    if (list.length == 13) {
      val surface = list(0);
      val leftId  = Util.toIntOption(list(1));
      val rightId = Util.toIntOption(list(2));
      val cost    = Util.toIntOption(list(3));
      if (leftId != None && rightId != None && cost != None) {
        return Some((surface, Array(leftId.get, rightId.get, cost.get)));
      }
    }
    return None;
  }

  def createPT(path: String, posMap: HashMap[String, Int]): PrefixTree[Array[Array[Int]]] = {
    Using[Source, PrefixTree[Array[Array[Int]]]](Source.fromFile(path)) { s =>
      val pt = PrefixTree[List[Array[Int]]](700000);
      s.getLines.foreach { line =>
        Util.parsePT(line, posMap) match {
          case None                  => ();
          case Some((surface, data)) => {
            pt.add[Array[Int]](surface, data) { (existing, elem) =>
              existing match {
                case ls: List[Array[Int]] => elem :: ls;
                case _                    => List[Array[Int]](elem);
              }
            };
          }
        }
      };
      pt.convertDataType[Array[Array[Int]]] { data =>
        data match {
          case ls @ x :: xs => ls.toArray;
          case _            => null;
        }
      };
    }
  }

  def checkPT(prefixree: PrefixTree[Array[Array[Int]]], ptPath: String): Unit = {
    val checkPT: Source => Boolean = s => {
      // 表層形,左文脈ID,右文脈ID,コスト,品詞,品詞細分類1,品詞細分類2,品詞細分類3,活用型,活用形,原形,読み,発音
      var cnt = 0;
      var ret = s.getLines.foldLeft(true) { (ret, line) =>
        Util.parsePT2(line) match {
          case None                  => ret;
          case Some((surface, data)) => {
            var output = "";
            val exists = prefixree.find(surface) match {
              case None     => {
                output = "None";
                false;
              }
              case Some(ds) => {
                output = ds.map("(" + _.mkString(",") + ")").mkString(", ");
                ds.exists(d => data(0) == d(0) && data(1) == d(1) && data(2) == d(2));
              }
            }
            if (exists) {
              ret;
            } else {
              println("input : %s, %s".format(surface, data.mkString(", ")));
              println("output : " + output);
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
