package com.github.ng3rdstmadgke.midomoji;

import java.text.Normalizer;
import scala.io.Source;
import scala.io.StdIn.readLine;

class Midomoji(private[this] val prefixtree: PrefixTree[Array[Array[Int]]],
               private[this] val matrix: Matrix,
               private[this] val charType: CharType) {
  private[this] val viterbi = new Viterbi(prefixtree, matrix, charType);
  val buffersize = 200000;

  def analyze(text: String): LatticeNode = {
    val normalized = Normalizer.normalize(text, Normalizer.Form.NFKC);
    val bos = viterbi.analyze(normalized);
    if (bos == None) {
      throw new RuntimeException("ノードが途中で途切れました (" + text + ")");
    } else {
      bos.get;
    }
  }

  def analyzeFile(path: String)(nodeToString: LatticeNode => String): Unit = {
    Using[Source, Unit](Source.fromFile(path)) { s =>
      val sb = new StringBuilder(buffersize + 10000);
      s.getLines.foreach { line =>
        if (sb.length() > buffersize) {
          println(sb);
          sb.setLength(0);
        }
        sb.append(nodeToString(analyze(line)));
      }
      println(sb);
    }
  }

  def analyzeStdin(nodeToString: LatticeNode => String): Unit = {
    val sb = new StringBuilder(buffersize + 10000);
    var line = readLine;
    while (line != null) {
      if (sb.length() > buffersize) {
        println(sb);
        sb.setLength(0);
      }
      sb.append(nodeToString(analyze(line)));
      line = readLine;
    }
    println(sb);
  }
}

object Midomoji {
  def format(fmt: String): LatticeNode => String = {
    fmt match {
      case "wakati"      => wakati;
      case "detail"      => detail();
      case "wakati-base" => wakatiBase()
      case _             => simple;
    }
  }

  def simple(nodes: LatticeNode): String = {
    val str = nodes.map(n => "%s\t%d\t%d\t%d".format(n.surface, n.leftId, n.rightId, n.genCost));
    "BOS\n" + str.mkString("\n") + "\nEOS\n";
  }

  def wakati(nodes: LatticeNode): String = {
    nodes.map(_.surface).mkString(" ") + "\n";
  }

  def detail(): LatticeNode => String = {
    val posInfo  = Util.kryoDeserializeFromResource[PosInfo](Util.posInfoBin());
    val metaInfo = Util.kryoDeserializeFromResource[MetaInfo](Util.metaInfoBin());
    nodes: LatticeNode => {
      val str = nodes.map { n =>
        val pos = posInfo.getPos(n.posId);
        val base = metaInfo.getBaseForm(n.id, n.surface);
        val yomi = metaInfo.getYomi(n.id, n.surface);
        "%s\t%d\t%d\t%d\t%s\t%s\t%s".format(n.surface, n.leftId, n.rightId, n.genCost, pos, base, yomi);
      }
      "BOS\n" + str.mkString("\n") + "\nEOS\n";
    }
  }

  def wakatiBase(): LatticeNode => String = {
    val metaInfo = Util.kryoDeserializeFromResource[MetaInfo](Util.metaInfoBin());
    nodes: LatticeNode => nodes.map(n => metaInfo.getBaseForm(n.id, n.surface)).mkString(" ") + "\n";
  }
}
