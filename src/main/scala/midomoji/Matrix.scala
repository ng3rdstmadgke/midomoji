package com.github.ng3rdstmadgke.midomoji

/**
 * 連接コスト表
 */
class Matrix(val leftSize: Int, val rightSize: Int) extends Serializable {
  private[this] val matrix = Array.ofDim[Int](leftSize, rightSize);
  private[this] val maxCost = 100000;

  def getCost(left: Int, right: Int): Int = try { matrix(left)(right); } catch { case e:ArrayIndexOutOfBoundsException => maxCost; }

  def setCost(left: Int, right: Int, cost: Int): Unit = matrix(left)(right) = cost;
}

object Matrix {
  def apply(leftSize: Int, rightSize: Int): Matrix = new Matrix(leftSize, rightSize);
}
