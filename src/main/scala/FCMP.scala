package fputil
import chisel3._
import chisel3.util._

// compare a and b, return lt, eq, le
class FCMP(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val lt = Output(Bool())
    val eq = Output(Bool())
    val le = Output(Bool())
  })

  val (a, b) = (io.a, io.b)
  val a_sign = a(width - 1)
  val b_sign = b(width - 1)
  val bothZero = a === 0.U && b === 0.U
  val a_minus_b = Cat(0.U(1.W), a) - Cat(0.U(1.W), b)
  val uint_eq = a_minus_b.tail(1) === 0.U
  val uint_less = a_sign ^ a_minus_b.head(1).asBool
  val sign_eq = a_sign === b_sign

  io.eq := uint_eq || bothZero
  io.le := Mux(
    sign_eq,
    uint_less || uint_eq,
    a_sign || bothZero
  )
  io.lt := Mux(
    sign_eq,
    uint_less && !uint_eq,
    a_sign && !bothZero
  )
}
