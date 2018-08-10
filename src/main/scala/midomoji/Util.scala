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
}
