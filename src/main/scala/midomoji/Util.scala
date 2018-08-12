package com.github.ng3rdstmadgke.midomoji

import scala.reflect.ClassTag;
import java.io.Closeable;
import scala.util.Try;
import java.io.{OutputStream, InputStream, ObjectOutputStream, ObjectInputStream, FileOutputStream, FileInputStream};
import java.nio.file.{Paths, Files};
import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Output;
import com.esotericsoftware.kryo.io.Input;

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

  // kryoを使ったシリアライズ(デフォルトコンストラクタを持たないクラスは扱えない)
  def kryoSerialize[A](obj: A, path: String): Unit = {
    Using[FileOutputStream, Unit](new FileOutputStream(path)) { fos =>
      Using[Output, Unit](new Output(fos)) { out =>
        val kryo = new Kryo();
        kryo.writeObject(out, obj);
      }
    }
  }

  // kryoを使ったデシリアライズ(デフォルトコンストラクタを持たないクラスは扱えない)
  def kryoDeserialize[A](path: String)(implicit tag: ClassTag[A]): A = {
    Using[FileInputStream, A](new FileInputStream(path)) { fis =>
      Using[Input, A](new Input(fis)) { in =>
        val kryo = new Kryo();
        kryo.readObject(in, tag.runtimeClass.asInstanceOf[Class[A]]);
      }
    }
  }

  // ビルトインのシリアライズ
  def builtinSerialize[A](obj: A, path: String): Unit = {
    Using[OutputStream, Unit](Files.newOutputStream(Paths.get(path))) { os =>
      Using[ObjectOutputStream, Unit](new ObjectOutputStream(os)) { oos =>
        oos.writeObject(obj);
      }
    }
  }

  // ビルトインのデシリアライズ
  def builtinDeserialize[A](path: String): A = {
    Using[InputStream, A](Files.newInputStream(Paths.get(path))) { is =>
      Using[ObjectInputStream, A](new ObjectInputStream(is)) { ois =>
        ois.readObject().asInstanceOf[A];
      }
    }
  }
}
