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
}
