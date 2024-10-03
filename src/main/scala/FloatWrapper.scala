package cintacc

import chisel3._
import chisel3.util.Cat

// Wraps a Chisel Flo or Dbl datatype to allow easy
// extraction of the different parts (sign, exponent, mantissa)

class FloatWrapper(val num: UInt) {
  val (sign: Bool, exponent: UInt, mantissa: UInt, zero: Bool) = num.getWidth match {
    case 32 =>
      (
        num(31).asBool,
        num(30, 23).asUInt,
        // if the exponent is 0
        // this is a denormalized number
        Cat(Mux(num(30, 23) === 0.U, 0.U(1.W), 1.U(1.W)), num(22, 0).asUInt),
        num(30, 0) === 0.U
      )

    case 64 =>
      (
        num(63).asBool,
        num(62, 52).asUInt,
        Cat(Mux(num(62, 52) === 0.U, 0.U(1.W), 1.U(1.W)), num(51, 0).asUInt),
        num(62, 0) === 0.U
      )
  }
}


