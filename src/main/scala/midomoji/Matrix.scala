package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;

/**
 * 連接コスト表
 */
class Matrix(private[this] val leftSize: Int, private[this] val rightSize: Int) extends Serializable {
  private[this] val matrix = new Array[Short](leftSize * rightSize);

  def this() = this(0, 0);

  def getCost(left: Int, right: Int): Int = try { matrix(left * leftSize + right).toInt; } catch { case e:ArrayIndexOutOfBoundsException => 65535; }

  def setCost(left: Int, right: Int, cost: Short): Unit = matrix(left * leftSize + right) = cost;
}

object Matrix {
  def apply(leftSize: Int, rightSize: Int): Matrix = new Matrix(leftSize, rightSize);

  def build(matrixPath: String): Matrix = {
    Using[Source, Matrix](Source.fromFile(matrixPath)) { s =>
      // 1行目はコスト表の縦と横の大きさが入っている。
      val iter = s.getLines;
      val Array(leftSize, rightSize) = if (iter.hasNext) iter.next().split("\t").map(_.toInt) else Array(0, 0);
      val mt = Matrix(leftSize, rightSize);
      iter.foreach { line =>
        val Array(l, r, c) = line.split("\t");
        mt.setCost(l.toInt, r.toInt, c.toShort);
      }
      mt;
    }
  }

  def check(matrix: Matrix, matrixPath: String): Unit = {
    Using[Source, Unit](Source.fromFile(matrixPath)) { s =>
      val iter = s.getLines;
      if (iter.hasNext) iter.next();
      iter.foreach { line =>
        val Array(l, r, c1) = line.split("\t").map(_.toInt);
        val c2 = matrix.getCost(l, r);
        if (c1 != c2) {
          println("left=%d, right=%d, FileCost=%d, ObjCost=%d".format(l, r, c1, c2));
          println("FAILED...");
          return ();
        }
      }
      println("OK!!!");
    }
  }
}
