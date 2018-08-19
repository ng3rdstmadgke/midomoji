package com.github.ng3rdstmadgke.midomoji;

import java.text.Normalizer;
import scala.io.Source;
import scala.io.StdIn.readLine;

class Midomoji(dictBin: String, matrixBin: String, configBin: String, posInfoBin: String, tokenMetaInfoBin: String) {
  private[this] val prefixtree    = PrefixTreeSerializeObject.deserialize[Array[Array[Int]]](dictBin);
  private[this] val matrix        = Util.kryoDeserialize[Matrix](matrixBin);
  private[this] val charType      = Util.kryoDeserialize[CharType](configBin);
  private[this] lazy val posInfo       = Util.kryoDeserialize[PosInfo](posInfoBin);
  private[this] lazy val tokenMetaInfo = Util.kryoDeserialize[TokenMetaInfo](tokenMetaInfoBin);
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
