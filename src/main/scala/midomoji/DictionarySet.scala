package com.github.ng3rdstmadgke.midomoji

import java.io.{OutputStream, InputStream, ObjectOutputStream, ObjectInputStream};
import java.nio.file.{Paths, Files};

@SerialVersionUID(1L)
class DictionarySet[A](val prefixtree: PrefixTree[A], val matrix: Matrix) extends Serializable {

  def serialize(dictPath: String): Unit = {
    Using[OutputStream, Unit](Files.newOutputStream(Paths.get(dictPath))) { os =>
      Using[ObjectOutputStream, Unit](new ObjectOutputStream(os)) { oos =>
        oos.writeObject(this);
      }
    }
  }
}

object DictionarySet {
  def apply[A](prefixtree: PrefixTree[A], matrix: Matrix): DictionarySet[A] = new DictionarySet(prefixtree, matrix);
  def apply[A](dictPath: String): DictionarySet[A] = {
    Using[InputStream, DictionarySet[A]](Files.newInputStream(Paths.get(dictPath))) { is =>
      Using[ObjectInputStream,DictionarySet[A]](new ObjectInputStream(is)) { ois =>
        ois.readObject().asInstanceOf[DictionarySet[A]];
      }
    }
  }
}
