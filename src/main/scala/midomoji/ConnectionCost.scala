package com.github.ng3rdstmadgke.midomoji

import java.io.{OutputStream, InputStream, ObjectOutputStream, ObjectInputStream};
import java.nio.file.{Paths, Files};

/**
 * 連接コスト表
 */
@SerialVersionUID(1L)
class ConnectionCost(val leftSize: Int, val rightSize: Int) extends Serializable {
  private[this] val matrix = Array.ofDim[Int](leftSize, rightSize);
  private[this] val maxCost = 100000;

  def getCost(left: Int, right: Int): Int = matrix(left)(right);
  //def getCost(left: Int, right: Int): Int = try { matrix(left)(right); } catch { case e:ArrayIndexOutOfBoundsException => maxCost; }

  def setCost(left: Int, right: Int, cost: Int): Unit = matrix(left)(right) = cost;

  def serialize(dictPath: String): Unit = {
    Using[OutputStream, Unit](Files.newOutputStream(Paths.get(dictPath))) { os =>
      Using[ObjectOutputStream, Unit](new ObjectOutputStream(os)) { oos =>
        oos.writeObject(this);
      }
    }
  }
}

object ConnectionCost {
  def apply(leftSize: Int, rightSize: Int): ConnectionCost = new ConnectionCost(leftSize, rightSize);
  def apply(dictPath: String): ConnectionCost = {
    Using[InputStream, ConnectionCost](Files.newInputStream(Paths.get(dictPath))) { is =>
      Using[ObjectInputStream, ConnectionCost](new ObjectInputStream(is)) { ois =>
        ois.readObject().asInstanceOf[ConnectionCost];
      }
    }
  }
}
