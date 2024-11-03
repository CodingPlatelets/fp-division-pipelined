// Süleyman Savas, Halmstad University
// 2017-02-13

/*
	If the inputs are given in the first cycle
	the result comes out at fifth cycle
 */

package fputil

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class FPDiv(val w: Int = 32) extends Module { //w = 32
  val io = IO(new Bundle {
    val in1 = Input(Valid(UInt(w.W)))
    val in2 = Input(Valid(UInt(w.W)))
    val out = Valid(UInt(w.W))
  })

  val inverter = Module(new fpInverter(23))
  val multiplier = Module(new FPMult(w))

  inverter.io.in := Pipe(io.in2.valid, io.in2.bits(22, 0), 0) //mantissa
  val invMantReg = RegNext(inverter.io.out) //stage 3

  val divLatency = 4
  // delay input1 due to the inverter
  val in1Reg = Pipe(io.in1.valid, io.in1.bits, divLatency) // stage 4
  // delay input2 exponent and sign due to the inverter
  val in2ExpReg = Pipe(io.in2.valid, io.in2.bits(30, 23), divLatency) // stage 4
  val in2SignReg = Pipe(io.in2.valid, io.in2.bits(31), divLatency) // stage 4

  val invMant = invMantReg
  val negExpTmp = 254.U - in2ExpReg.bits
  val negExp = Mux(invMant.bits === 0.U, negExpTmp, negExpTmp - 1.U)

  // we should raise an execption if both mantissa and exponent are zero (the final result should be inf

  multiplier.io.a := in1Reg
  multiplier.io.b.bits := Cat(in2SignReg.bits, negExp, invMant.bits(23, 1))
  multiplier.io.b.valid := in2SignReg.valid && invMant.valid
  // skipping msb of inverter (multiplying mantissa by 2)

  io.out <> multiplier.io.res
}

object FPDiv extends App {
  ChiselStage.emitSystemVerilogFile(
    new FPDiv,
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info"),
    args = Array("--target-dir", "./build/chisel")
  )
}
