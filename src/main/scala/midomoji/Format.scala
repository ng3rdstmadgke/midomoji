package com.github.ng3rdstmadgke.midomoji;

object Format {
  def format(fmt: String): (String, LatticeNode) => String = {
    fmt match {
      case "wakati"      => wakati;
      case "detail"      => detail();
      case "wakati-base" => wakatiBase();
      case _             => simple;
    }
  }

  def simple(text: String, nodes: LatticeNode): String = {
    val str = nodes.map(n => "%s\t%d\t%d".format(text.slice(n.startIdx, n.endIdx), Morpheme.connId(n.morpheme), Morpheme.connId(n.morpheme)));
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
        val morpheme = n.morpheme;
        val surface = text.slice(n.startIdx, n.endIdx);
        val pos = posInfo.getPos(Morpheme.posId(morpheme));
        val base = metaInfo.getBaseForm(Morpheme.id(morpheme), surface);
        val yomi = metaInfo.getYomi(Morpheme.id(morpheme), surface);
        "%s\t%d\t%d\t%s\t%s\t%s\t%d".format(surface, Morpheme.connId(morpheme), Morpheme.genCost(morpheme), pos, base, yomi, n.totalCost);
      }
      "BOS\n" + str.mkString("\n") + "\nEOS\n";
    }
  }

  def wakatiBase(): (String, LatticeNode) => String = {
    val metaInfo = Util.kryoDeserializeFromResource[MetaInfo](Util.metaInfoBin());
    (text: String, nodes: LatticeNode) => nodes.map(n => metaInfo.getBaseForm(Morpheme.id(n.morpheme), text.slice(n.startIdx, n.endIdx))).mkString(" ") + "\n";
  }
}
