package org.perso

import scodec.bits.ByteVector

import java.nio.file.{Files, Paths}

object Main extends App {
  val bytes = Files.readAllBytes(Paths.get("data/encoded8077/8077-3843821349-2022040516225517-22293.anonym.dat"))
  val encoded = TLVManager.splitMessage(ByteVector(bytes))

  println(encoded.length)

}
