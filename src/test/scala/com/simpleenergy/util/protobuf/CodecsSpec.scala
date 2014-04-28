package com.simpleenergy.util.protobuf

import scodec.bits._
import scodec.Codec

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import scalaz.\/-
import scalaz.std.string._
import org.specs2.matcher.Parameters

/**
 * TODO: Add docs
 */
class CodecsSpec extends Specification with ScalaCheck {
  "RawVarInt" should {
    "properly decode known values" in {
      // These specs use the raw bit vectors returned. Conversions to Int require padding
      RawVarInt.decode(bin"00000001") mustEqual \/-((BitVector.empty, bin"0000001"))
      RawVarInt.decode(bin"1010110000000010") mustEqual \/-((BitVector.empty, bin"00000100101100"))
      RawVarInt.decode(bin"100000001000000000000010") mustEqual \/-((BitVector.empty, bin"000001000000000000000"))
    }

    "properly encode known valus" in {
      RawVarInt.encode(bin"00000001") mustEqual \/-(bin"00000001")
      RawVarInt.encode(bin"0000000100101100") mustEqual \/-(bin"1010110000000010")
      RawVarInt.encode(hex"ffffffff".toBitVector) mustEqual \/-(bin"1111111111111111111111111111111100001111")
    }
  }

  def testInt[A](codec: Codec[A])(i: A) = {
    val result = for {
      bits <- codec.encode(i)
      //_ = println(s"Encoded $i to ${bits.toBin}")
      (remain, ival) <- codec.decode(bits)
    } yield (remain, ival)

    result mustEqual \/-((BitVector.empty, i))
  }

  def testLong(l: Long) = {

  }

  override implicit def defaultParameters: Parameters = Parameters(minTestsOk = 1000)

  // We don't cover uints, doubles or floats here because those are just aliases for existing scodec Codecs
  "Codecs" should {
    "handle boundary values" in {
      testInt(Codecs.Int32)(-1)

      testInt(Codecs.SInt32)(-1374760782)

      Codecs.SInt32.encode(Int.MinValue) mustEqual \/-(bin"1111111111111111111111111111111100001111")
      Codecs.SInt32.encode(Int.MaxValue) mustEqual \/-(bin"1111111011111111111111111111111100001111")
    }

    "round-trip Int32" in check {
      testInt(Codecs.Int32) _
    }

    "round-trip Int64" in check {
      testInt(Codecs.Int64) _
    }

    "round-trip SInt32" in check {
      testInt(Codecs.SInt32) _
    }

    "round-trip SInt64" in check {
      testInt(Codecs.SInt64) _
    }

    "round-trip Bytes" in check {
      a: Array[Byte] =>

      val result = for {
        rawBits <- Codecs.Bytes.encode(a)
        decoded <- Codecs.Bytes.decode(rawBits)
      } yield decoded

      result.toOption.get._2.toSeq must containAllOf(a.toSeq).exactly
    }
  }
}
