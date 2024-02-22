package org.perso

import org.scalatest.funspec.AnyFunSpec
import scodec.bits.*


class TLVManagerTest extends AnyFunSpec {
  describe("computeLength") {
    it("should yield 04 when length is 8004") {
      assert(TLVManager.computeLength(hex"8004").endsWith(hex"04"))
    }
  }

  describe("splitMessage") {
    it("should find 2 entities") {
      val bytes = hex"807780040011223380578003556677"

      assert(TLVManager.splitMessage(bytes).length == 2)
    }
  }
}
