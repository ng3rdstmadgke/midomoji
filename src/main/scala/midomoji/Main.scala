package com.github.ng3rdstmadgke.midomoji

import scala.io.Source;
import scala.io.StdIn.readLine;
import scala.collection.mutable.ListBuffer;
import java.nio.file.{Paths, Files};

object Main {
  def main(args: Array[String]): Unit = {
    args.toList match {
      case ("-p" | "--build-prefixtree") :: srcPath :: dstPath :: xs => {
        println("--build-prefixtree");
      }
      case ("-c" | "--build-connectioncost") :: srcPath :: dstPath :: xs => {
        // -c /mnt/mecab-ipadic-2.7.0-20070801/matrix.def ./dictionary/connectioncost.bin
        val connectionCost = Using[Source, ConnectionCost](Source.fromFile(srcPath)) { s =>
          val fileIter = s.getLines;
          // 1行目はコスト表の縦と横の大きさがスペース区切りで入っている。
          val Array(leftSize, rightSize) = if (fileIter.hasNext) fileIter.next().split(" ").map(_.trim.toInt).slice(0, 2) else Array(0, 0);
          val cc = ConnectionCost(leftSize, rightSize);
          fileIter.foreach { line =>
            val Array(left, right, cost) = line.split(" ").map(_.trim.toInt).slice(0, 3);
            cc.setCost(left, right, cost);
          }
          cc;
        }
        connectionCost.serialize(dstPath);
        // 正しく登録されているかチェック
        def f(s: Source): Boolean = {
          val cc = ConnectionCost(dstPath);
          s.getLines.foreach { line =>
            val arr = line.split(" ").map(_.trim.toInt).slice(0, 3);
            if (arr.length == 3) {
              val Array(left, right, cost) = arr;
              if (cost != cc.getCost(left, right)) {
                print("[err] left=%d, right=%d, file=%d, bin=%d".format(left, right, cost, cc.getCost(left, right)));
                return false;
              }
            }
          }
          return true;
        }
        if (Using[Source, Boolean](Source.fromFile(srcPath))(f)) {
          println("build ConnectionCost complete !!")
        } else {
          println("build ConnectionCost failed ...");
          Files.deleteIfExists(Paths.get(dstPath));
        }
      }
      case ("-d" | "--debug") :: xs => {
        debug();
      }
      case ("-a" | "--analysis") :: xs  => {
        println("解析");
      }
      case _ => {
        val help = ListBuffer[String]();
        help += "-p, --build-prefixtree [SRC_PATH] [DST_PATH]     : 辞書の構築";
        help += "-c, --build-connectioncost [SRC_PATH] [DST_PATH] : 連接コスト表の構築";
        help += "-d, --debug                                      : デバッグモード起動";
        help += "-h, --help                                       : ヘルプの表示";
        println(help.mkString("\n"));
      }
    }
  }

  def debug(): Unit = {
    print("command : ");
    readLine.split(" ").toList match {
      case "exit" :: xs => {
        return ();
      }
      case _ => {
        val help = ListBuffer[String]();
        help += "exit                                 : デバッグモードを終了する";
        println(help.mkString("\n"));
      }
    }
    debug();
  }
}
