package com.github.ng3rdstmadgke.midomoji

import scala.reflect.ClassTag;

class PrefixTreeSerializeObject[A](var size: Int, var base: Array[Int], var check: Array[Int], var data: Array[A]) {
  def this() = this(0, null, null, null);
}

object PrefixTreeSerializeObject {
  def serialize[A](prefixtree: PrefixTree[A], path: String): Unit = {
    val obj = new PrefixTreeSerializeObject[A](prefixtree.size, prefixtree.base, prefixtree.check, prefixtree.data);
    Util.kryoSerialize[PrefixTreeSerializeObject[A]](obj, path);
  }

  def deserialize[A](path: String)(implicit c: ClassTag[A]): PrefixTree[A] = {
    val obj = Util.kryoDeserialize[PrefixTreeSerializeObject[A]](path);
    new PrefixTree[A](obj.size, obj.base, obj.check, obj.data);
  }
}
