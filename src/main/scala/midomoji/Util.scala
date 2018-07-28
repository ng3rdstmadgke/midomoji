package com.github.ng3rdstmadgke.midomoji

import java.io.Closeable;
import scala.util.Try;

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

  def parsePT(input: String): Option[Morpheme] = {
    val list    = input.split(",");
    if (list.length == 13) {
      val leftId  = Util.toIntOption(list(1));
      val rightId = Util.toIntOption(list(2));
      val cost    = Util.toIntOption(list(3));
      if (leftId != None && rightId != None && cost != None) {
        val morpheme = Morpheme(list(0), leftId.get, rightId.get, cost.get, list(4), list(10), list(11));
        return Some(morpheme);
      }
    }
    return None;
  }
}
