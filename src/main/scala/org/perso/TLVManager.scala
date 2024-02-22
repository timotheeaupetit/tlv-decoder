package org.perso


import scodec.bits.*

object TLVManager extends App {
  val bytes = hex"807780040011223380578003556677"

  def computeLength(bytes: ByteVector): ByteVector = {
    val inter1 = bytes & hex"00ff"  // AND
    val inter2 = inter1 & hex"7fff"  // AND
    val inter3 = inter1 ^ inter2  // XOR
    val mask = inter3 >> 1  // shift 1 bit

    mask ^ inter1  // XOR
  }

  def lengthToLong(l: ByteVector): Long = computeLength(l).toLong()

  def splitMessage(bytes: ByteVector): List[ByteVector] = ???

}
