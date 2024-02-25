package org.perso

import org.scalatest.funspec.AnyFunSpec
import scodec.bits.*


class TLVManagerTest extends AnyFunSpec {
  describe("computeLength") {
    it("should yield 39c when length is 871c") {
      assert(TLVManager.computeLength(hex"871c").endsWith(hex"39c"))
    }
    it("should yield 32a when length is 862a") {
      assert(TLVManager.computeLength(hex"862a").endsWith(hex"32a"))
    }
    it("should yield 2ec when length is 856c") {
      assert(TLVManager.computeLength(hex"856c").endsWith(hex"2ec"))
    }
    it("should yield 277 when length is 8477") {
      assert(TLVManager.computeLength(hex"8477").endsWith(hex"277"))
    }
    it("should yield 19b when length is 831b") {
      assert(TLVManager.computeLength(hex"831b").endsWith(hex"19b"))
    }
    it("should yield 101 when length is 8201") {
      assert(TLVManager.computeLength(hex"8201").endsWith(hex"101"))
    }
    it("should yield b2 when length is 8132") {
      assert(TLVManager.computeLength(hex"8132").endsWith(hex"b2"))
    }
    it("should yield 34 when length is 8034") {
      assert(TLVManager.computeLength(hex"8034").endsWith(hex"34"))
    }
  }

  describe("lenthToLong") {
    it("should yield 5 when length is 0x8005") {
      assert(TLVManager.lengthToLong(hex"8005") == 5)
    }
    it("should yield 631 for 0x8477") {
      assert(TLVManager.lengthToLong(hex"8477") == 631)
    }
    it("should yield 627 for 0x8473") {
      assert(TLVManager.lengthToLong(hex"8473") == 627)
    }
  }

  describe("splitMessage") {
    it("should find 2 entities") {
      val bytes = hex"805780040011223380578003556677"
      val result = TLVManager.splitMessage(bytes)
      assert(result.length == 2)
    }

    it("should ignore unknown tags") {
      val bytes = hex"80778004001122338666800377335580778003556677"
      val result = TLVManager.splitMessage(bytes)
      assert(result.length == 2)
      assert(result.forall(_.startsWith(hex"8077")))
    }
  }
}
