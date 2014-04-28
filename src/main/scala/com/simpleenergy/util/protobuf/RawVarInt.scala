package com.simpleenergy.util.protobuf

import scala.annotation.tailrec

import scalaz._

import scodec._
import scodec.bits._

/**
 * This codec deals with the varint encoding used by protobuf (see https://developers.google.com/protocol-buffers/docs/encoding#varints)
 *
 * Further transform to an actual int value (e.g. int32, sint64) must be handled externally
 */
object RawVarInt extends Codec[BitVector] {
  override def decode(bits: BitVector): String \/ (BitVector, BitVector) = {
    @tailrec
    def runBits(accumulator: BitVector, remaining: BitVector): String \/ (BitVector, BitVector) = remaining.headOption match {
      case Some(true) =>
        val (current, rest) = remaining.splitAt(8)
        //println(s"current = ${current.toBin}, rest = ${rest.toBin}, accumulator = ${accumulator.toBin}")

        val toPrepend = current.slice(1, 8)
        //println(s"  toPrepend = ${toPrepend.toBin}")
        runBits(toPrepend ++ accumulator, rest)

      case Some(false) =>
        val toPrepend = remaining.slice(1, 8)
        val finalBits = toPrepend ++ accumulator
        //println(s"toPrepend = ${toPrepend.toBin}, accumulator = ${accumulator.toBin}, final = ${finalBits.toBin}")
        \/-((remaining.drop(8), finalBits))

      case None =>
        -\/("Ran out of bits for varint")
    }

    runBits(BitVector.empty, bits)
  }

  final val highBit = hex"80".toBitVector

  override def encode(value: BitVector): String \/ BitVector = {
    @tailrec
    def runBits(accumulator: BitVector, remaining: BitVector): String \/ BitVector =
      if (remaining.nonEmpty) {
        val (rest, toEncode) = remaining.splitAt(remaining.size - 7)

        val needsMoreBytes = rest.populationCount > 0

        if (needsMoreBytes) {
          // FIXME: This is a workaround due to some issues with scodec (https://github.com/scodec/scodec-bits/issues/11)
          val flagged = bin"1" ++ toEncode
          //println(s"Encoded ${toEncode.toBin} to ${flagged.toBin} with needsMore = $needsMoreBytes")
          runBits(accumulator ++ flagged, rest)
        } else {
          \/-(accumulator ++ toEncode.padLeft(8))
        }
      } else {
        \/-(accumulator)
      }

    if (value.size < 8) {
      -\/("Need at least one byte to encode a raw varInt")
    } else if (value.size % 8 != 0) {
      -\/(s"Input must be byte-aligned. Found ${value.size} bits")
    } else {
      runBits(BitVector.empty, value)
    }
  }
}
