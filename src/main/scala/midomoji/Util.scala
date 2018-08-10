package com.github.ng3rdstmadgke.midomoji

import java.io.Closeable;
import scala.util.Try;
import java.io.{OutputStream, InputStream, ObjectOutputStream, ObjectInputStream};
import java.nio.file.{Paths, Files};

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

  def serialize[A](obj: A, path: String): Unit = {
    Using[OutputStream, Unit](Files.newOutputStream(Paths.get(path))) { os =>
      Using[ObjectOutputStream, Unit](new ObjectOutputStream(os)) { oos =>
        oos.writeObject(obj);
      }
    }
  }

  def deserialize[A](path: String): A = {
    Using[InputStream, A](Files.newInputStream(Paths.get(path))) { is =>
      Using[ObjectInputStream, A](new ObjectInputStream(is)) { ois =>
        ois.readObject().asInstanceOf[A];
      }
    }
  }
}
