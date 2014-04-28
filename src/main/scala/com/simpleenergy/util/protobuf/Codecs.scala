package com.simpleenergy.util.protobuf

import scala.annotation.tailrec

import scalaz.{\/-, -\/, \/}
import scalaz.std.boolean._
import scalaz.std.string._
import scalaz.syntax.std.boolean._
import scalaz.syntax.id._
import scalaz.syntax.std.option._

import scodec.{Codec, Decoder}
import scodec.bits._
import scodec.codecs._

/**
 */
object Codecs {

  /**
   * Defines the base wiretype corresponding to protobuf encoding
   * @param value The value of the wiretype encoding, held in the last three bits of the field marker varint
   * @param skipDecoder This decoder is used to skip unknown fields
   */
  sealed abstract class WireType(val value: Int, skipDecoder: Decoder[_])

  case object VarInt extends WireType(0, RawVarInt)
  case object Bits64 extends WireType(1, bits(64))
  case object LengthDelimited extends WireType(2, variableSizeBytes(Int32, Identity, 0))
  @deprecated("Groups in Protobuf are deprecated")
  case object StartGroup extends WireType(3, DecodeUntilEndGroup)
  @deprecated("Groups in Protobuf are deprecated")
  case object EndGroup extends WireType(4, ignore(0))
  case object Bits32 extends WireType(5, bits(32))

  private final val DecodeUntilEndGroup = new Decoder[Unit] {
    override def decode(bits: BitVector): \/[String, (BitVector, Unit)] = ???
  }

  /**
   * Used to ignore an input entirely
   */
  private final val Identity = new Codec[BitVector] {
    override def decode(bits: BitVector): \/[String, (BitVector, BitVector)] = \/-((BitVector.empty, bits))
    override def encode(value: BitVector): \/[String, BitVector] = \/-(value)
  }
  

  private lazy final val wireTypeMap = List(VarInt, Bits64, LengthDelimited, StartGroup, EndGroup, Bits32).map {
    v => (v.value, v)
  }.toMap

  case class FieldMarker(id: Int, wireType: WireType)

  val field = new Decoder[FieldMarker] {
    override def decode(bits: BitVector): \/[String, (BitVector, FieldMarker)] =
      for {
        (remainder, fieldBits) <- RawVarInt.decode(bits)
        (_, fieldId) <- uint((fieldBits.size - 3).toInt).decode(fieldBits.shiftRight(3, signExtension = false))
        (_, wireTypeValue) <- uint(3).decode(fieldBits.takeRight(3))
        wireType <- wireTypeMap.get(wireTypeValue).toRightDisjunction(s"Could not determine wire type for value $wireTypeValue")
      } yield (remainder, FieldMarker(fieldId, wireType))
  }

  class VarInt[A](fullBits: Int, intCodec: Codec[A]) extends Codec[A] {
    override def decode(bits: BitVector): \/[String, (BitVector, A)] = RawVarInt.decode(bits) flatMap {
      case (remainder, varbits) =>
        // Due to the way varints encode (multiples of 7), we may end up with more or less bits than we need for the intX codec
        val adjusted = if (varbits.size > fullBits) varbits.takeRight(fullBits) else varbits.padLeft(fullBits)

        intCodec.complete.decode(adjusted).map {
          case (_, i) => (remainder, i)
        }
    }

    override def encode(value: A): \/[String, BitVector] =
      for {
        rawBits <- intCodec.encode(value)
        varBits <- RawVarInt.encode(rawBits)
      } yield varBits
  }

  val Int32 = new VarInt[Int](32, int32)
  val Int64 = new VarInt[Long](64, int64)

  // JVM doesn't have unsigned numbers...
  val UInt32 = Int32
  val UInt64 = Int64

  val SInt32 = Int32.xmap[Int]({ n => (n >>> 1) ^ -(n & 1) },{ n => (n << 1) ^ (n >> 31) })

  val SInt64 = Int64.xmap[Long]({ n => (n >>> 1) ^ -(n & 1) },{ n => (n << 1) ^ (n >> 63) })

  val Fixed32 = int32L
  val Fixed64 = int64L

  val Double = doubleL
  val Float  = floatL

  val Bytes = variableSizeBytes(Int32, bytes.xmap(_.toArray, { ba: Array[Byte] => ByteVector(ba) }))
}


