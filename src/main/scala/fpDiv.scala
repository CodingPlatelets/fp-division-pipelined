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
    val in1 = Input(UInt(w.W))
    val in2 = Input(UInt(w.W))
    val out = Output(UInt(w.W))
  })

  val inverter = Module(new fpInverter(23))
  val multiplier = Module(new FPMult(w))

  inverter.io.in1 := io.in2(22, 0) //mantissa
  // delay input1 due to the inverter
  val in1Reg0 = RegNext(io.in1) // stage 0
  val in1Reg1 = RegNext(in1Reg0) // stage 1
  val in1Reg2 = RegNext(in1Reg1) // stage 2
  val in1Reg3 = RegNext(in1Reg2) // stage 3

  // delay input2 exponent and sign due to the inverter
  val in2ExpReg0 = RegNext(io.in2(30, 23)) // stage 0
  val in2SignReg0 = RegNext(io.in2(31)) // stage 0
  val in2ExpReg1 = RegNext(in2ExpReg0) // stage 1
  val in2SignReg1 = RegNext(in2SignReg0) // stage 1
  val in2ExpReg2 = RegNext(in2ExpReg1) // stage 2
  val in2SignReg2 = RegNext(in2SignReg1) // stage 2
  val in2ExpReg3 = RegNext(in2ExpReg2) // stage 3
  val in2SignReg3 = RegNext(in2SignReg2) // stage 3
  val invMantReg = RegNext(inverter.io.out(23, 0)) //stage 3

  val invMant = invMantReg
  val negExpTmp = 254.U - in2ExpReg3
  val negExp = Mux(invMant === 0.U, negExpTmp, negExpTmp - 1.U)

  // we should raise an execption if both mantissa and exponent are zero (the final result should be inf

  multiplier.io.a := in1Reg3
  multiplier.io.b := Cat(in2SignReg3, negExp, invMant(23, 1))
  // skipping msb of inverter (multiplying mantissa by 2)

  io.out := multiplier.io.res
}

object FPDiv extends App {
  ChiselStage.emitSystemVerilogFile(
    new FPDiv,
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info"),
    args = Array("--target-dir", "./build/chisel")
  )
}
