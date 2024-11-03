// Süleyman Savas, 2016-12-15
// Halmstad University

package fputil

import chisel3._
import chisel3.util._
//import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}

class fpInverter(val w: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Valid(UInt(w.W)))
    val out = Valid(UInt((w + 1).W))

  })

  val inverterLatency = 3
  io.out.valid := ShiftRegister(io.in.valid, inverterLatency)

  // instantiate lookup tables
  val tableC = Module(new lookupC())
  val tableL = Module(new lookupL())
  val tableJ = Module(new lookupJ())

  // Most significant 9 bits of the input (mantissa) is used as address
  // to the coefficient lookup tables
  val coeffAddr = io.in.bits(w - 1, w - 6)
  tableC.io.addr := coeffAddr
  tableL.io.addr := coeffAddr
  tableJ.io.addr := coeffAddr

  val sub1 = (io.in.bits ^ Fill(23, 1.U)) + 1.U

  val mul1 = Module(new VarSizeMul(23, 17, 24))
  mul1.io.in1 := tableJ.io.out
  mul1.io.in2 := io.in.bits(w - 7, 0)
  // result will be input to adder1

  val mul2 = Module(new VarSizeMul(24, 17, 29))
  mul2.io.in1 := tableC.io.out
  mul2.io.in2 := (io.in.bits(w - 7, 0) * io.in.bits(w - 7, 0))(33, 17)
  // result will be input to sub2

  // workaround registers (sub1 reg, mul1 reg and mul2 reg does not delay their input)
  val w_mul1_reg = RegNext(mul1.io.out)
  val w_mul2_reg = RegNext(mul2.io.out)

  // stage1 registers with workaround
  val tableL_out_reg = RegNext(tableL.io.out)

  val mul2_out_reg = RegNext(w_mul2_reg)

// using sub due to the sign value of the j coefficients
  val sub2 = Module(new VarSizeSub(27, 27, 27))
  val sub2_in2 = (w_mul1_reg ^ Fill(24, 1.U)) + 1.U
  sub2.io.in2 := Cat(sub2_in2, 0.U(3.W))
  sub2.io.in1 := tableL_out_reg
  // result will be input to sub2

//	using an adder due to the sign value of the c coefficients
  val adder = Module(new VarSizeAdder(29, 29, 25))
  adder.io.in1 := RegNext(Cat(sub2.io.out, 0.U(2.W)))
  adder.io.in2 := mul2_out_reg
  // result will be input to mul3

  //stage2 registers
  val sub1_out_reg2 = ShiftRegister(sub1, inverterLatency)
  val adder_out_reg = RegNext(adder.io.out)

  val mul3 = Module(new VarSizeMul(w, 25, 24))
  mul3.io.in1 := sub1_out_reg2
  mul3.io.in2 := adder_out_reg

  io.out.bits := mul3.io.out
}
