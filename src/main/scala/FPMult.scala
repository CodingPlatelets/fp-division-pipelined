// Takes 2 clock cyles to produce the result

package fputil

import chisel3._
import chisel3.util._
//import FloatUtils.{floatToBigInt, doubleToBigInt}

class MantissaRounder(val n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Bits(n.W))
    val out = Output(Bits((n - 1).W))
  })

  io.out := io.in(n - 1, 1) // + io.in(0)
}

// can pipe
class FPMult(val n: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(Valid(UInt(n.W)))
    val b = Input(Valid(UInt(n.W)))
    val res = Valid(UInt(n.W))
  })

  val mulLatency = 1
  io.res.valid := ShiftRegister(io.a.valid && io.b.valid, mulLatency)

  val a_wrap = new FloatWrapper(io.a.bits)
  val b_wrap = new FloatWrapper(io.b.bits)

  val stage1_sign = a_wrap.sign ^ b_wrap.sign
  val stage1_exponent = a_wrap.exponent + b_wrap.exponent
  val stage1_mantissa = a_wrap.mantissa * b_wrap.mantissa
  val stage1_zero = a_wrap.zero || b_wrap.zero

  val sign_reg = RegNext(stage1_sign)
  val exponent_reg = RegNext(stage1_exponent)
  val mantissa_reg = RegNext(stage1_mantissa)
  val zero_reg = RegNext(stage1_zero)

  val stage2_sign = sign_reg
  val stage2_exponent = Wire(UInt(a_wrap.exponent.getWidth.W))
  val stage2_mantissa = Wire(UInt((a_wrap.mantissa.getWidth - 1).W))

  val (mantissaLead, mantissaSize, exponentSize, exponentSub) = n match {
    case 32 => (47, 23, 8, 127)
    case 64 => (105, 52, 11, 1023)
  }

  val rounder = Module(new MantissaRounder(mantissaSize + 1))

  when(zero_reg) {
    stage2_exponent := 0.U(exponentSize.W)
    rounder.io.in := 0.U((mantissaSize + 1).W)
  }.elsewhen(mantissa_reg(mantissaLead) === 1.U) {
    stage2_exponent := exponent_reg - (exponentSub - 1).U
    rounder.io.in := mantissa_reg(mantissaLead - 1, mantissaLead - mantissaSize - 1)
  }.otherwise {
    stage2_exponent := exponent_reg - (exponentSub).U
    rounder.io.in := mantissa_reg(mantissaLead - 2, mantissaLead - mantissaSize - 2)
  }

  stage2_mantissa := rounder.io.out

  io.res.bits := Cat(stage2_sign.asUInt, stage2_exponent.asUInt, stage2_mantissa.asUInt)
}

class FPMult32 extends FPMult(32) {}
class FPMult64 extends FPMult(64) {}
