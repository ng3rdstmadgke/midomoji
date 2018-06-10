package com.github.ng3rdstmadgke.midomoji;

import org.scalatest.{FlatSpec, DiagrammedAssertions};
import java.nio.file.{Paths, Files};

class ConnectionCostSpec extends FlatSpec with DiagrammedAssertions {
  "ConnectionCost" should "値の登録と取り出し" in {
    val cc = ConnectionCost(10, 10);
    cc.setCost(0, 0, 50);
    cc.setCost(9, 9, 100);
    assert(cc.getCost(0, 0) == 50);
    assert(cc.getCost(9, 9) == 100);
  }

  it should "シリアライズ・デシリアライズ" in {
    val cc = ConnectionCost(10, 10);
    cc.setCost(0, 0, 50);
    cc.setCost(9, 9, 100);
    val dictPath = "dictionary/test_connectioncost.bin";
    cc.serialize(dictPath);
    val cc2 = ConnectionCost(dictPath);
    assert(cc2.getCost(0, 0) == 50);
    assert(cc2.getCost(9, 9) == 100);
    Files.deleteIfExists(Paths.get(dictPath));
  }
}
