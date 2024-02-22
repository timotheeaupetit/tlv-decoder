package org.perso


import scodec.bits.*

import scala.annotation.tailrec

object TLVManager extends App {
  def computeLength(bytes: ByteVector): ByteVector = {
    val inter1 = bytes and hex"ff00"
    val inter2 = inter1 and hex"7fff"
    val inter3 = inter2 >> 1 // Shift 1 bit to the right
    val mask = inter3 ^ inter1 // XOR

    mask ^ bytes // XOR
  }

  def lengthToLong(l: ByteVector): Long = computeLength(l).toLong()

  @tailrec
  def splitMessage(bytes: ByteVector, acc: List[ByteVector] = List()): List[ByteVector] = bytes.consume(4)(v => v.acquire(4)) match
    case Right(x) =>
      val (tag, length) = x._2.splitAt(2)
      val nbBytes = lengthToLong(length)
      x._1.consume(nbBytes)(v => v.acquire(nbBytes)) match
        case Right(y) if Set(hex"8057", hex"8077").contains(tag) => splitMessage(y._1, acc :+ (tag ++ length ++ y._2))
        case Right(y) => splitMessage(y._1, acc)
        case Left(_) => splitMessage(x._1, acc)
    case Left(_) => acc

}
