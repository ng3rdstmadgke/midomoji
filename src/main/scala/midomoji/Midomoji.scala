package com.github.ng3rdstmadgke.midomoji;

import java.text.Normalizer;
import java.io.{BufferedWriter, BufferedReader, OutputStreamWriter, InputStreamReader, OutputStream, InputStream, FileOutputStream, FileInputStream};
import java.nio.charset.StandardCharsets;

class Midomoji(private[this] val prefixtree: PrefixTree[Array[Array[Int]]],
               private[this] val matrix: Matrix,
               private[this] val charType: CharType) {
  private[this] val lattice = new Lattice(prefixtree, charType, matrix);

  def analyzeInput(is: InputStream, os: OutputStream, bs: Int = 8192)(nodeToString: (String, LatticeIterator) => String): Unit = {
    Using[BufferedReader, Unit](new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8), bs)) { br =>
      Using[BufferedWriter, Unit](new BufferedWriter(new OutputStreamWriter(os, StandardCharsets.UTF_8), bs)) { bw =>
        var line = br.readLine();
        val sLattice = lattice;
        while (line != null) {
          val normalized = Normalizer.normalize(line, Normalizer.Form.NFKC);
          val output = nodeToString(normalized, sLattice.analyze(normalized));
          bw.write(output);
          line = br.readLine();
        }
        bw.flush();
      }
    }
  }
}

object Midomoji {
  def format(fmt: String): (String, LatticeIterator) => String = {
    fmt match {
      case "wakati"      => wakati;
      case "detail"      => detail();
      case "wakati-base" => wakatiBase()
      case _             => simple;
    }
  }

  def simple(text: String, nodes: LatticeIterator): String = {
    val str = nodes.map { n =>
      val Array(startIdx, endIdx, leftId, rightId, genCost, posId, id, totalCost, nextNodeIdx) = n;
      val surface = text.slice(startIdx, endIdx);
      "%s\t%d\t%d\t%d".format(surface, leftId, rightId, genCost);
    };
    "BOS\n" + str.mkString("\n") + "\nEOS\n";
  }

  def wakati(text: String, nodes: LatticeIterator): String = {
    nodes.map( n => text.slice(n(0), n(1))).mkString(" ") + "\n";
  }

  def detail(): (String, LatticeIterator) => String = {
    val posInfo  = Util.kryoDeserializeFromResource[PosInfo](Util.posInfoBin());
    val metaInfo = Util.kryoDeserializeFromResource[MetaInfo](Util.metaInfoBin());
    (text: String, nodes: LatticeIterator) => {
      val str = nodes.map { n =>
        val Array(startIdx, endIdx, leftId, rightId, genCost, posId, id, totalCost, nextNodeIdx) = n;
        val surface = text.slice(startIdx, endIdx);
        val pos = posInfo.getPos(posId);
        val base = metaInfo.getBaseForm(id, surface);
        val yomi = metaInfo.getYomi(id, surface);
        "%s\t%d\t%d\t%d\t%s\t%s\t%s".format(surface, leftId, rightId, genCost, pos, base, yomi);
      }
      "BOS\n" + str.mkString("\n") + "\nEOS\n";
    }
  }

  def wakatiBase(): (String, LatticeIterator) => String = {
    val metaInfo = Util.kryoDeserializeFromResource[MetaInfo](Util.metaInfoBin());
    (text: String, nodes: LatticeIterator) => {
      nodes.map {n =>
        val Array(startIdx, endIdx, leftId, rightId, genCost, posId, id, totalCost, nextNodeIdx) = n;
        val surface = text.slice(startIdx, endIdx);
        metaInfo.getBaseForm(id, surface)
      }.mkString(" ") + "\n";
    }
  }
}
