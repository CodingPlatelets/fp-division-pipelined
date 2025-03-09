package fputil.example

import chisel3._
import chisel3.util._

import fputil._
import fputil.utils.bf2fp
import fputil.utils.fp2bf

// bf16 for pre, vec is a N dim vector
class VecMul(size: Int = 32, val precision: Int = 16) extends Module {
  val io = IO(new Bundle {
    val in1 = Input(Valid(Vec(size, UInt(precision.W))))
    val in2 = Input(Valid(Vec(size, UInt(precision.W))))
    val out = Valid(UInt(precision.W))
  })

  val mulUnits = VecInit.fill(size)(Module(new FPMult(precision)).io)
  mulUnits.foreach(_.a.valid := io.in1.valid)
  mulUnits.foreach(_.b.valid := io.in2.valid)
  for (i <- 0 until size) {
    mulUnits(i).a.bits := io.in1.bits(i)
    mulUnits(i).b.bits := io.in2.bits(i)
  }
  class middle_data extends Bundle {
    val bits:  UInt = UInt((2 * precision).W)
    val valid: Bool = Bool()
  }

  val middle_reg = Reg(Vec(size, new middle_data))

  val all_done = mulUnits.map(_.res.valid).reduce(_ & _)
  for (i <- 0 until size) {
    middle_reg(i).bits := Cat(mulUnits(i).res.bits, 0.U(precision.W))
    middle_reg(i).valid := all_done
  }

  val res = middle_reg.reduceTree(
    { (a, b) =>
      {
        val adder = Module(new FPAdd32)
        adder.io.a.bits := a.bits
        adder.io.b.bits := b.bits
        adder.io.a.valid := a.valid
        adder.io.b.valid := b.valid

        val result = Wire(new middle_data)
        result.bits := adder.io.res.bits
        result.valid := adder.io.res.valid
        result
      }
    },
    (x) => RegNext(x)
  )

  val fp2bfModule = Module(new fp2bf)
  fp2bfModule.io.in := res.bits
  io.out.bits := fp2bfModule.io.out
  io.out.valid := res.valid
}
