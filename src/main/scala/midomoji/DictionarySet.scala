package com.github.ng3rdstmadgke.midomoji

import java.io.{OutputStream, InputStream, ObjectOutputStream, ObjectInputStream};
import java.nio.file.{Paths, Files};

@SerialVersionUID(1L)
class DictionarySet(val prefixtree: PrefixTree[Morpheme], val matrix: Matrix) extends Serializable {

  def serialize(dictPath: String): Unit = {
    Using[OutputStream, Unit](Files.newOutputStream(Paths.get(dictPath))) { os =>
      Using[ObjectOutputStream, Unit](new ObjectOutputStream(os)) { oos =>
        oos.writeObject(this);
      }
    }
  }
}

object DictionarySet {
  def apply(prefixtree: PrefixTree[Morpheme], matrix: Matrix): DictionarySet = new DictionarySet(prefixtree, matrix);
  def apply(dictPath: String): DictionarySet = {
    Using[InputStream, DictionarySet](Files.newInputStream(Paths.get(dictPath))) { is =>
      Using[ObjectInputStream,DictionarySet](new ObjectInputStream(is)) { ois =>
        ois.readObject().asInstanceOf[DictionarySet];
      }
    }
  }
}
