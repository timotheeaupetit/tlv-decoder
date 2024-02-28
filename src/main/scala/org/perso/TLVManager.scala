package org.perso


import scodec.bits.*

import scala.annotation.tailrec

object TLVManager extends App {
  private val headers = Set[ByteVector](hex"8077", hex"8057")

  /** Decode bytes one by one as ASCII characters, then concatenate them to form a string
   * @param bytes ByteVector to decode
   * @return the decoded string
   */
  def toAscii(bytes: ByteVector): String = bytes.foldLeft("")((acc, next) => s"$acc${next.toChar}")

  /** Decode bytes as an integer, then convert it to string
   * If parsing fails, yield an empty string
   * @param bytes ByteVector to decode
   * @return the decoded integer as string
   */
  def toInt(bytes: ByteVector): String = bytes.toInt(signed = false).toString

  /** Decode bytes as an epoch timestamp, then convert it to string.
   * @param bytes ByteVector to decode
   * @return the decoded long integer as string
   */
  def toLongTimestamp(bytes: ByteVector): String = {
    val diff = bytes.toLong(signed = false).longValue() - 2208988800L
    diff.toString
  }

  def computeLength(bytes: ByteVector): ByteVector = {
    val inter1 = bytes & hex"ff00" // AND
    val inter2 = inter1 & hex"7fff" // AND
    val inter3 = inter2 >> 1 // shift 1 bit to the right
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
        case Right(y) if headers.contains(tag) => splitMessage(y._1, acc :+ (tag ++ length ++ y._2))
        case Right(y) => splitMessage(y._1, acc)
        case Left(_) => splitMessage(x._1, acc)
    case Left(_) => acc

  /** Decode a single sequence from TLV to key-value pairs
   *
   * @param bytes     ByteVector to decode
   * @param parentTag upper level tag containing bytes (level 1 tags have an empty parentTag)
   * @param kv        key-value pairs of decoded tags within parentTag
   * @return a sequence as key-value pairs of decoded tags
   */
  @tailrec
  final def decodeSequence(bytes: ByteVector,
                           parentTag: ByteVector = ByteVector.empty,
                           kv: Map[String, String] = Map()): Map[String, String] = bytes match {
    case ByteVector.empty => kv
    case v =>
      val tag = v.take(2)
      val length = v.slice(2, 4)
      val nbBytes = lengthToLong(length)
      val tail = v.drop((tag ++ length).length)
      val (value, remain) = tail.splitAt(nbBytes)

      parentTag match {
        case ByteVector.empty => tag match { // level 1
          case t if headers.contains(t) => decodeSequence(value, t, kv) // go to level 2
          case _ => decodeSequence(remain, parentTag, kv) // ignore every other level 1 tags
        }
        case pt if headers.contains(pt) => tag match { // level 2
          case t if t == hex"8002" => decodeSequence(value, t, kv) // go to level 3
          case _ => decodeSequence(remain, pt, kv) // ignore every other level 2 tags
        }
        case pt if pt == hex"8002" => tag match { // level 3
          case t if t == hex"8003" => decodeSequence(remain, pt, kv + ("8003_3" -> toInt(value)))
          case t if t == hex"8004" => decodeSequence(remain, pt, kv ++ decodeLevel4(value, t))
          case t if t == hex"8005" => decodeSequence(remain, pt, kv + ("8005_3" -> toAscii(value)))
          case t if t == hex"8006" => decodeSequence(remain, pt, kv + ("8006_3" -> toAscii(value)))
          case t if t == hex"800a" => decodeSequence(remain, pt, kv + ("800a_3" -> toLongTimestamp(value)))
          case t if t == hex"800c" => decodeSequence(remain, pt, kv + ("800c_3" -> toLongTimestamp(value)))
          case t if t == hex"800d" => decodeSequence(remain, pt, kv + ("800d_3" -> toLongTimestamp(value)))
          case t if t == hex"800e" => decodeSequence(remain, pt, kv + ("800e_3" -> toInt(value)))
          case t if t == hex"8011" => decodeSequence(remain, pt, kv + ("8011_3" -> toInt(value)))
          case t if t == hex"8012" => decodeSequence(remain, pt, kv + ("8012_3" -> toInt(value)))
          case t if t == hex"8013" => decodeSequence(remain, pt, kv + ("8013_3" -> toInt(value)))
          case t if t == hex"8014" => decodeSequence(remain, pt, kv + ("8014_3" -> toInt(value)))
          case t if t == hex"8015" => decodeSequence(remain, pt, kv + ("8015_3" -> toAscii(value)))
          case t if t == hex"8016" => decodeSequence(remain, pt, kv + ("8016_3" -> toAscii(value)))
          case t if t == hex"8017" => decodeSequence(remain, pt, kv + ("8017_3" -> toAscii(value)))
          case t if t == hex"8019" => decodeSequence(remain, pt, kv + ("8019_3" -> toAscii(value)))
          case t if t == hex"801a" => decodeSequence(remain, pt, kv + ("801a_3" -> toAscii(value)))
          case t if t == hex"801b" => decodeSequence(remain, pt, kv + ("801b_3" -> toAscii(value)))
          case t if t == hex"801c" => decodeSequence(remain, pt, kv + ("801c_3" -> toInt(value)))
          case t if t == hex"801d" => decodeSequence(remain, pt, kv + ("801d_3" -> toInt(value)))
          case t if t == hex"801e" => decodeSequence(remain, pt, kv + ("801e_3" -> toInt(value)))
          case t if t == hex"801f" => decodeSequence(remain, pt, kv + ("801f_3" -> toAscii(value)))
          case t if t == hex"8021" => decodeSequence(remain, pt, kv + ("8021_3" -> toAscii(value)))
          case t if t == hex"8030" => decodeSequence(remain, pt, kv ++ decodeLevel4(value, t))
          case t if t == hex"8050" => decodeSequence(remain, pt, kv + ("8050_3" -> toAscii(value)))
          case t if t == hex"8052" => decodeSequence(remain, pt, kv ++ decodeLevel4(value, t))
          case t if t == hex"8070" => decodeSequence(remain, pt, kv ++ decodeLevel4(value, t))
          case t if t == hex"8071" => decodeSequence(remain, pt, kv + ("8071_3" -> toAscii(value)))
          case t if t == hex"8073" => decodeSequence(remain, pt, kv + ("8073_3" -> toAscii(value)))
          case t if t == hex"8074" => decodeSequence(remain, pt, kv + ("8074_3" -> toAscii(value)))
          case t if t == hex"8077" => decodeSequence(remain, pt, kv + ("8077_3" -> toAscii(value)))
          case t if t == hex"8078" => decodeSequence(remain, pt, kv + ("8078_3" -> toAscii(value)))
          case t if t == hex"807e" => decodeSequence(remain, pt, kv ++ decodeLevel4(value, t))
          case t if t == hex"8120" => decodeSequence(remain, pt, kv + ("8120_3" -> toAscii(value)))
          case t if t == hex"8121" => decodeSequence(remain, pt, kv + ("8121_3" -> toAscii(value)))
          case t if t == hex"8122" => decodeSequence(remain, pt, kv + ("8122_3" -> toAscii(value)))
          case t if t == hex"8123" => decodeSequence(remain, pt, kv + ("8123_3" -> toAscii(value)))
          case t if t == hex"8124" => decodeSequence(remain, pt, kv + ("8124_3" -> toAscii(value)))
          case t if t == hex"8125" => decodeSequence(remain, pt, kv + ("8125_3" -> toAscii(value)))
          case t if t == hex"8126" => decodeSequence(remain, pt, kv + ("8126_3" -> toAscii(value)))
          case t if t == hex"8127" => decodeSequence(remain, pt, kv + ("8127_3" -> toAscii(value)))
          case t if t == hex"8128" => decodeSequence(remain, pt, kv + ("8128_3" -> toAscii(value)))
          case t if t == hex"8129" => decodeSequence(remain, pt, kv + ("8129_3" -> toAscii(value)))
          case t if t == hex"8130" => decodeSequence(remain, pt, kv + ("8130_3" -> toAscii(value)))
          case t if t == hex"8131" => decodeSequence(remain, pt, kv + ("8131_3" -> toAscii(value)))
          case t if t == hex"8132" => decodeSequence(remain, pt, kv + ("8132_3" -> toAscii(value)))
          case t if t == hex"8133" => decodeSequence(remain, pt, kv + ("8133_3" -> toAscii(value)))
          case t if t == hex"8134" => decodeSequence(remain, pt, kv + ("8134_3" -> toAscii(value)))
          case _ => decodeSequence(remain, pt, kv) // ignore every other level 3 tags
        }
        case _ => kv // ignore every other tags
      }
  }

  /** Decode level 4 tags from TLV to key-value pairs
   *
   * @param bytes     ByteVector to decode
   * @param parentTag a level 3 tag containing bytes
   * @param kv        key-value pairs of decoded tags within parentTag
   * @return key-value pairs of decoded level 4 tags
   */
  @tailrec
  final def decodeLevel4(bytes: ByteVector,
                         parentTag: ByteVector,
                         kv: Map[String, String] = Map()): Map[String, String] = bytes match {
    case ByteVector.empty => kv
    case v =>
      val tag = v.take(2)
      val length = v.slice(2, 4)
      val nbBytes = lengthToLong(length)
      val tail = v.drop((tag ++ length).length)
      val (value, remain) = tail.splitAt(nbBytes)

      parentTag match {
        case pt if pt == hex"8004" => tag match {
          case t if t == hex"8001" => decodeLevel4(remain, pt, kv + ("8004_8001_4" -> toAscii(value)))
          case _ => kv // ignore all other level 4 tags within that level 3 tag
        }
        case pt if pt == hex"8030" => tag match {
          case t if t == hex"8000" => decodeLevel4(remain, pt, kv + ("8030_8000_4" -> toAscii(value)))
          case t if t == hex"8002" => decodeLevel4(remain, pt, kv + ("8030_8002_4" -> toAscii(value)))
          case t if t == hex"8003" => decodeLevel4(remain, pt, kv + ("8030_8003_4" -> toAscii(value)))
          case t if t == hex"8004" => decodeLevel4(remain, pt, kv + ("8030_8004_4" -> toLongTimestamp(value)))
          case t if t == hex"8005" => decodeLevel4(remain, pt, kv + ("8030_8005_4" -> toAscii(value)))
          case t if t == hex"8006" => decodeLevel4(remain, pt, kv + ("8030_8006_4" -> toInt(value)))
          case t if t == hex"8007" => decodeLevel4(remain, pt, kv + ("8030_8007_4" -> toAscii(value)))
          case t if t == hex"8008" => decodeLevel4(remain, pt, kv + ("8030_8008_4" -> toAscii(value)))
          case t if t == hex"8009" => decodeLevel4(remain, pt, kv + ("8030_8009_4" -> toAscii(value)))
          case t if t == hex"800a" => decodeLevel4(remain, pt, kv + ("8030_800a_4" -> toInt(value)))
          case t if t == hex"800b" => decodeLevel4(remain, pt, kv + ("8030_800b_4" -> toInt(value)))
          case _ => kv // ignore all other level 4 tags within that level 3 tag
        }
        case pt if pt == hex"8052" => tag match {
          case t if t == hex"8000" => decodeLevel4(remain, pt, kv + ("8052_8000_4" -> toLongTimestamp(value)))
          case t if t == hex"8001" => decodeLevel4(remain, pt, kv + ("8052_8001_4" -> toAscii(value)))
          case t if t == hex"8002" => decodeLevel4(remain, pt, kv + ("8052_8002_4" -> toAscii(value)))
          case t if t == hex"8003" => decodeLevel4(remain, pt, kv + ("8052_8003_4" -> toAscii(value)))
          case t if t == hex"8004" => decodeLevel4(remain, pt, kv + ("8052_8004_4" -> toAscii(value)))
          case t if t == hex"8005" => decodeLevel4(remain, pt, kv + ("8052_8005_4" -> toAscii(value)))
          case t if t == hex"8006" => decodeLevel4(remain, pt, kv + ("8052_8006_4" -> toAscii(value)))
          case t if t == hex"8007" => decodeLevel4(remain, pt, kv + ("8052_8007_4" -> toAscii(value)))
          case t if t == hex"8008" => decodeLevel4(remain, pt, kv + ("8052_8008_4" -> toAscii(value)))
          case t if t == hex"8009" => decodeLevel4(remain, pt, kv + ("8052_8009_4" -> toAscii(value)))
          case t if t == hex"800a" => decodeLevel4(remain, pt, kv + ("8052_800a_4" -> toAscii(value)))
          case t if t == hex"800b" => decodeLevel4(remain, pt, kv + ("8052_800b_4" -> toAscii(value)))
          case t if t == hex"800c" => decodeLevel4(remain, pt, kv + ("8052_800c_4" -> toInt(value)))
          case _ => kv // ignore all other level 4 tags within that level 3 tag
        }
        case pt if pt == hex"8070" => tag match {
          case t if t == hex"8000" => decodeLevel4(remain, pt, kv + ("8070_8000_4" -> toAscii(value)))
          case t if t == hex"8001" => decodeLevel4(remain, pt, kv + ("8070_8001_4" -> toAscii(value)))
          case _ => kv // ignore all other level 4 tags within that level 3 tag
        }
        case pt if pt == hex"807e" => tag match {
          case t if t == hex"8050" => decodeLevel4(remain, pt, kv + ("807e_8050_4" -> toAscii(value)))
          case t if t == hex"8061" => decodeLevel4(remain, pt, kv + ("807e_8061_4" -> toAscii(value)))
          case _ => kv // ignore all other level 4 tags within that level 3 tag
        }
        case _ => kv // ignore all other level 3 tags
      }
  }
}
