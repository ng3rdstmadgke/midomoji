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

  def getNormalizedPath(path: String): String = Paths.get(path).normalize.toString;

  // build前のファイル
  def morphemeTsv(dir: String = ""): String = getNormalizedPath(dir + "/morpheme.tsv");
  def matrixTsv(dir: String = ""): String   = getNormalizedPath(dir + "/matrix.tsv");
  def charTsv(dir: String = ""): String     = getNormalizedPath(dir + "/char.tsv");
  def charTypeTsv(dir: String = ""): String = getNormalizedPath(dir + "/char_type.tsv");
  def unkTsv(dir: String = ""): String      = getNormalizedPath(dir + "/unk.tsv");
  def posTsv(dir: String = ""): String      = getNormalizedPath(dir + "/pos.tsv");
  // build後のファイル
  def dictBin(dir: String = ""): String     = getNormalizedPath(dir + "/dict.bin");
  def matrixBin(dir: String = ""): String   = getNormalizedPath(dir + "/matrix.bin");
  def configBin(dir: String = ""): String   = getNormalizedPath(dir + "/config.bin");
  def posInfoBin(dir: String = ""): String  = getNormalizedPath(dir + "/pos_info.bin");
  def metaInfoBin(dir: String = ""): String = getNormalizedPath(dir + "/meta_info.bin");

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

  // kryoを使ったデシリアライズ(デフォルトコンストラクタを持たないクラスは扱えない)
  def kryoDeserializeFromResource[A](path: String)(implicit tag: ClassTag[A]): A = {
    Using[InputStream, A](getClass.getResourceAsStream(path)) { fis =>
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
