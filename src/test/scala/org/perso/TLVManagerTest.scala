package org.perso

import org.scalatest.funspec.AnyFunSpec
import scodec.bits.*


class TLVManagerTest extends AnyFunSpec {
  describe("toAscii") {
    it("should decode bytes to Ascii") {
      assert(TLVManager.toAscii(hex"737061726b") == "spark")
    }
    it("should yield an empty string when bytes is empty") {
      assert(TLVManager.toAscii(ByteVector.empty) == "")
    }
  }

  describe("toInt") {
    it("should decode hex'00000000' to '0'") {
      assert(TLVManager.toInt(hex"00000000") === "0")
    }
    it("should decode hex'00000014' to '20'") {
      assert(TLVManager.toInt(hex"00000014") === "20")
    }
    it("should decode hex'00000123' to '291'") {
      assert(TLVManager.toInt(hex"00000123") === "291")
    }
  }

  describe("toLongTimestamp") {
    it("should convert hexadecimal string to string timestamp") {
      assert(TLVManager.toLongTimestamp(hex"E5F6CE23") === "1649168291")
    }
  }

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

  describe("decodeLevel4") {
    it("should interpret 8004 tag content") {
      val _8004 = hex"8001801A2B3333393939393939393939405858585858582D5858582E6672"

      val result = TLVManager.decodeLevel4(_8004, hex"8004")
      assert(result.size == 1)
      assert(result.keys.exists(_ == "8004_8001_4"))
      assert(!result.keys.exists(_ == "8004_8002_4"))
    }

    it("should interpret 8030 tag content") {
      val _8030 = hex"8007800C393939393939393939393A39"

      val result = TLVManager.decodeLevel4(_8030, hex"8030")
      assert(result.size == 1)
      assert(result("8030_8007_4") == "9999999999:9")
      assert(!result.keys.exists(_.contains("800e")))
    }

    it("should ignore irrelevant 800D child tag within tag 8052") {
      val _8052 = hex"80008004E5F6CE23800D80020001"

      val result = TLVManager.decodeLevel4(_8052, hex"8052")
      assert(result.size == 1)
      assert(result("8052_8000_4") === "1649168291")
      assert(!result.keys.exists(_.contains("800d")))
    }

    it("should get multiple children tags within tag 8052") {
      val _8052 = hex"8001800AE5F6CE23E5F6CE234422800680020001800C800400000013"

      val result = TLVManager.decodeLevel4(_8052, hex"8052")
      assert(result.size == 3)
      assert(result.keys.exists(_ == "8052_8001_4"))
      assert(result.keys.exists(_ == "8052_8006_4"))
      assert(result.keys.exists(_ == "8052_800c_4"))
      assert(!result.keys.exists(_.contains("8002")))
      assert(!result.keys.exists(_.contains("800a")))
    }

    it("should interpret 8070 tag content") {
      val _8070 = hex"8000801B585858585858585858585858582E5858585858582D5858582E66728001800E585858585858585858582D707269"

      val result = TLVManager.decodeLevel4(_8070, hex"8070")
      assert(result.size == 2)
      assert(result.keys.exists(_ == "8070_8001_4"))
      assert(!result.keys.exists(_.endsWith("_3")))
      assert(result("8070_8000_4") === "XXXXXXXXXXXXX.XXXXXX-XXX.fr")
    }

    it("should interpret 807E BIV tag content") {
      val _807e = hex"805080065052494D535F"

      val result = TLVManager.decodeLevel4(_807e, hex"807E")
      assert(result.size == 1)
      assert(result("807e_8050_4") === "PRIMS_")
      assert(!result.keys.exists(_.endsWith("_3")))
    }

    it("should return the pre-existing key-value pair when 807E is empty") {
      val _807e = ByteVector.empty

      val result = TLVManager.decodeLevel4(_807e, hex"807E", Map[String, String]("807e_8050_4" -> "PRIMS_"))
      assert(result.size == 1)
      assert(result.keys.exists(_ == "807e_8050_4"))
    }

    it("should interpret 807E BTIC tag content") {
      val _807e = hex"805080065052494D535F8061800136"

      val result = TLVManager.decodeLevel4(_807e, hex"807E")
      assert(result.size == 2)
      assert(result.keys.exists(_ == "807e_8050_4"))
      assert(result("807e_8061_4") === "6")
      assert(!result.keys.exists(_.contains("8050_3")))
    }
  }

  describe("decodeSequence") {
    it("should ignore irrelevant level 1 tag 8666") {
      val lvl1 = hex"8666800255118077800C80028008800E800400000123"

      val result = TLVManager.decodeSequence(lvl1)
      assert(result.size == 1)
      assert(result.keys.exists(_ == "800e_3"))
    }

    it("should ignore irrelevant level 2 tag 8088") {
      val lvl2 = hex"80888002551180028008800E800400000123"

      val result = TLVManager.decodeSequence(lvl2, hex"8057")
      assert(result.size == 1)
      assert(result.keys.exists(_ == "800e_3"))
    }

    it("should ignore irrelevant level 3 tag 8042") {
      val lvl3 = hex"80748004FFDDEE22804280025511"

      val result = TLVManager.decodeSequence(lvl3, hex"8002")
      assert(result.size == 1)
      assert(result.keys.exists(_ == "8074_3"))
    }

    it("should get level 3 tag 8077") {
      val lvl3 = hex"80778004FFDDEE22"

      val result = TLVManager.decodeSequence(lvl3, hex"8002")
      assert(result.size == 1)
      assert(result.keys.exists(_ == "8077_3"))
    }

    it("should interpret level 3 and 4 tag contents for BIV") {
      val lvl3 = hex"800A8004E5F6CE238071800672656D6F74658126800130807E800A805080065052494D535F"

      val result = TLVManager.decodeSequence(lvl3, hex"8002")
      assert(result.size == 4)
      assert(result.keys.exists(_ == "8126_3"))
      assert(result.keys.exists(_ == "807e_8050_4"))
      assert(result("800a_3") === "1649168291")
      assert(result("8071_3") === "remote")
      assert(!result.keys.exists(_ == "8003_3"))
    }

    it("should interpret level 3 and 4 tag contents for BTIC") {
      val lvl3 = hex"8003800400000003807E800F805080065052494D535F8061800136800D8004E5F6902E8133801A2B3333393939393939393939405858585858582D5858582E66728134800E5072696D61727920446576696365"

      val result = TLVManager.decodeSequence(lvl3, hex"8002")
      assert(result.size == 6)
      assert(result.keys.exists(_ == "8003_3"))
      assert(result.keys.exists(_ == "800d_3"))
      assert(result.keys.exists(_ == "807e_8061_4"))
      assert(result("8133_3") === "+33999999999@XXXXXX-XXX.fr")
      assert(result("8134_3") === "Primary Device")
      assert(!result.keys.exists(_ == "8004_3"))
    }
  }
}
