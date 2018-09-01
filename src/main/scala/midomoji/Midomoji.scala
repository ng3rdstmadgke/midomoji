package com.github.ng3rdstmadgke.midomoji;

import java.text.Normalizer;
import java.io.{BufferedWriter, BufferedReader, OutputStreamWriter, InputStreamReader, OutputStream, InputStream, FileOutputStream, FileInputStream};
import java.nio.charset.StandardCharsets;

class Midomoji(private[this] val prefixtree: PrefixTree[Array[Array[Int]]],
               private[this] val matrix: Matrix,
               private[this] val charType: CharType) {
  private[this] val viterbi = new Viterbi(prefixtree, matrix, charType);

  def analyzeInput(is: InputStream, os: OutputStream, bs: Int = 8192)(nodeToString: (String, LatticeNode) => String): Unit = {
    Using[BufferedReader, Unit](new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8), bs)) { br =>
      Using[BufferedWriter, Unit](new BufferedWriter(new OutputStreamWriter(os, StandardCharsets.UTF_8), bs)) { bw =>
        val sViterbi = viterbi;
        var line = br.readLine();
        while (line != null) {
          val normalized = Normalizer.normalize(line, Normalizer.Form.NFKC);
          bw.write(nodeToString(normalized, sViterbi.analyze(normalized)));
          line = br.readLine();
        }
        bw.flush();
      }
    }
  }
}

object Midomoji {
  def format(fmt: String): (String, LatticeNode) => String = {
    fmt match {
      case "wakati"      => wakati;
      case "detail"      => detail();
      case "wakati-base" => wakatiBase()
      case _             => simple;
    }
  }

  def simple(text: String, nodes: LatticeNode): String = {
    val str = nodes.map(n => "%s\t%d\t%d\t%d".format(text.slice(n.startIdx, n.endIdx), n.leftId, n.rightId, n.genCost));
    "BOS\n" + str.mkString("\n") + "\nEOS\n";
  }

  def wakati(text: String, nodes: LatticeNode): String = {
    nodes.map(n => text.slice(n.startIdx, n.endIdx)).mkString(" ") + "\n";
  }

  def detail(): (String, LatticeNode) => String = {
    val posInfo  = Util.kryoDeserializeFromResource[PosInfo](Util.posInfoBin());
    val metaInfo = Util.kryoDeserializeFromResource[MetaInfo](Util.metaInfoBin());
    (text: String, nodes: LatticeNode) => {
      val str = nodes.map { n =>
        val surface = text.slice(n.startIdx, n.endIdx);
        val pos = posInfo.getPos(n.posId);
        val base = metaInfo.getBaseForm(n.id, surface);
        val yomi = metaInfo.getYomi(n.id, surface);
        "%s\t%d\t%d\t%d\t%s\t%s\t%s".format(surface, n.leftId, n.rightId, n.genCost, pos, base, yomi);
      }
      "BOS\n" + str.mkString("\n") + "\nEOS\n";
    }
  }

  def wakatiBase(): (String, LatticeNode) => String = {
    val metaInfo = Util.kryoDeserializeFromResource[MetaInfo](Util.metaInfoBin());
    (text: String, nodes: LatticeNode) => nodes.map(n => metaInfo.getBaseForm(n.id, text.slice(n.startIdx, n.endIdx))).mkString(" ") + "\n";
  }
}
