package com.github.ng3rdstmadgke.midomoji;

import org.scalatest.{FlatSpec, DiagrammedAssertions};
import java.nio.file.{Paths, Files};

class MatrixSpec extends FlatSpec with DiagrammedAssertions {
  "Matrix" should "値の登録と取り出し" in {
    val mt = Matrix(10, 10);
    mt.setCost(0, 0, 50);
    mt.setCost(9, 9, 100);
    assert(mt.getCost(0, 0) == 50);
    assert(mt.getCost(9, 9) == 100);
  }
}
