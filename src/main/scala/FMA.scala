package fputil
import chisel3._
import chisel3.util._

// a * b + c
// four cycles latency
class FMA(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val a = Input(Valid(UInt(width.W)))
    val b = Input(Valid(UInt(width.W)))
    val c = Input(Valid(UInt(width.W)))
    val res = Valid(UInt(width.W))
  })

  // because of the latency of FPAdd, we need to register the input c
  val (a, b, c) = (io.a, io.b, RegNext(io.c))

  // one cycle latency
  val mulRes = FPMult(width)(a.bits, b.bits, a.valid && b.valid)

  // three cycle latency
  val tmpRes = FPAdd(width)(c.bits, mulRes.bits, c.valid && mulRes.valid)

  io.res.bits := Mux(tmpRes.valid, tmpRes.bits, c.bits)
  io.res.valid := tmpRes.valid
}
