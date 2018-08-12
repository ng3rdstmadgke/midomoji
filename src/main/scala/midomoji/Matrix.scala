package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;

/**
 * 連接コスト表
 */
class Matrix(val leftSize: Int, val rightSize: Int) extends Serializable {
  private[this] val matrix = Array.ofDim[Int](leftSize, rightSize);
  private[this] val maxCost = 100000;

  def this() = this(0, 0);

  def getCost(left: Int, right: Int): Int = try { matrix(left)(right); } catch { case e:ArrayIndexOutOfBoundsException => maxCost; }

  def setCost(left: Int, right: Int, cost: Int): Unit = matrix(left)(right) = cost;
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
        val Array(l, r, c) = line.split("\t").map(_.toInt);
        mt.setCost(l, r, c);
      }
      mt;
    }
  }

  def check(matrix: Matrix, matrixPath: String): Unit = {
    // 正しく登録されているかチェック
    val errors = Using[Source, List[String]](Source.fromFile(matrixPath)) { s =>
      val msgTpl = "left=%d, right=%d, FileCost=%d, ObjCost=%d";
      val iter = s.getLines;
      if (iter.hasNext) iter.next();
      iter.foldLeft(List[String]()) { (es, line) =>
        val Array(l, r, c) = line.split("\t").map(_.toInt);
        val objCost = matrix.getCost(l, r);
        if (c == objCost) es else msgTpl.format(l, r, c, objCost) :: es;
      }
    }
    errors match {
      case Nil => println("OK!!!");
      case ls  => println(ls.mkString("\n"));
    }
  }
}
