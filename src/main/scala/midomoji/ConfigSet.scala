package com.github.ng3rdstmadgke.midomoji;

import java.io.{OutputStream, InputStream, ObjectOutputStream, ObjectInputStream};
import java.nio.file.{Paths, Files};

@SerialVersionUID(1L)
class ConfigSet(val charType: CharType) extends Serializable {
  def serialize(confPath: String): Unit = {
    Using[OutputStream, Unit](Files.newOutputStream(Paths.get(confPath))) { os =>
      Using[ObjectOutputStream, Unit](new ObjectOutputStream(os)) { oos =>
        oos.writeObject(this);
      }
    }
  }
}

object ConfigSet {
  def deserialize(confPath: String): ConfigSet = {
    Using[InputStream, ConfigSet](Files.newInputStream(Paths.get(confPath))) { is =>
      Using[ObjectInputStream, ConfigSet](new ObjectInputStream(is)) { ois =>
        ois.readObject().asInstanceOf[ConfigSet];
      }
    }
  }
}
