package fputil.utils
import chisel3._
import chisel3.util._

// convert fp32 to bf16
class fp2bf() extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(UInt(16.W))
  })

  // 提取 FP32 结构
  val sign = io.in(31)
  val exponent = io.in(30, 23)
  val mantissa = io.in(22, 0) // 23 位尾数

  // BF16 尾数部分 (7-bit)，直接截取 FP32 高 7 位
  val truncatedMantissa = mantissa(22, 16)

  // FP32 低 16 位，用于舍入
  val roundBits = mantissa(15, 0)

  // 舍入策略: 向最近偶数舍入
  val roundBit = roundBits(15) // 最高位，用于判断是否需要进位
  val roundSticky = roundBits(14, 0).orR // 低 15 位是否有非零，决定是否进位
  val addOne = roundBit && (roundSticky || truncatedMantissa(0))

  // 计算最终 BF16 尾数，考虑进位
  val roundedMantissa = truncatedMantissa + addOne

  // 处理指数进位溢出的情况
  val mantissaOverflow = (roundedMantissa === 0.U) && addOne
  val finalExponent = Mux(mantissaOverflow, exponent + 1.U, exponent)

  // 组合 BF16 结果
  io.out := Cat(sign, finalExponent, roundedMantissa)
}

// convert bf16 to fp32
class bf2fp() extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(16.W))
    val out = Output(UInt(32.W))
  })


  val sign = io.in(15)
  val exponent = io.in(14, 7)
  val mantissa = io.in(6, 0)

  val extendedMantissa = Cat(mantissa, 0.U(16.W))

  io.out := Cat(sign, exponent, extendedMantissa)
}
