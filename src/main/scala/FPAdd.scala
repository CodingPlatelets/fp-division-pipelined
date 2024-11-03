/*
	Stage1 : find the difference between exponents
	Stage2 : shift the proper mantissa
	Stage3 : Add/subtract mantissa, check overflow
	Stage4 : Normalize mantissa and exponent
 */
package fputil

import chisel3._
import chisel3.util.Cat
import chisel3.util.Reverse
import chisel3.util.PriorityEncoder
import chisel3.util.Valid
import chisel3.util.Pipe
import FloatUtils.{doubleAdd, doubleToBigInt, floatAdd, floatToBigInt, getExpMantWidths}

// create output Bundle with valid signal
class stage1Out(val expWidth: Int, val mantWidth: Int) extends Bundle {
  val b_larger = Output(Bool())
  val mant_shift = Output(UInt(expWidth.W))
  val exp = Output(UInt(expWidth.W))
  val manta = Output(UInt((mantWidth + 1).W))
  val mantb = Output(UInt((mantWidth + 1).W))
  val sign = Output(Bool())
  val sub = Output(Bool())
}

// Stage 1: 计算指数差，确定移位量和最终符号
class FPAddStage1(val n: Int) extends Module {
  val (expWidth, mantWidth) = getExpMantWidths(n)

  val io = IO(new Bundle {
    val a = Input(Valid(UInt(n.W)))
    val b = Input(Valid(UInt(n.W)))

    val out = Valid(new stage1Out(expWidth, mantWidth))
  })

  // 包装输入的浮点数以便提取各个部分
  val a_wrap = new FloatWrapper(io.a.bits)
  val b_wrap = new FloatWrapper(io.b.bits)

  // 扩展指数位以防止减法溢出
  val ext_exp_a = Cat(0.U(1.W), a_wrap.exponent)
  val ext_exp_b = Cat(0.U(1.W), b_wrap.exponent)
  val exp_diff = ext_exp_a - ext_exp_b

  // 寄存器存储尾数和操作类型
  val reg_manta = RegNext(a_wrap.mantissa)
  val reg_mantb = RegNext(b_wrap.mantissa)
  val reg_sub = RegNext(a_wrap.sign ^ b_wrap.sign) // 异号为减法

  // 控制寄存器
  val reg_sign = Reg(Bool())
  val reg_mant_shift = Reg(UInt(expWidth.W))
  val reg_exp = Reg(UInt(expWidth.W))

  // 根据指数差判断大小数,并计算移位量和最终符号
  val exp_diff_sign = exp_diff(expWidth).asBool
  val reg_b_larger = RegNext(exp_diff_sign)

  // 根据大小数选择移位量、指数和符号
  reg_mant_shift := Mux(exp_diff_sign, -exp_diff(expWidth - 1, 0), exp_diff(expWidth - 1, 0))
  reg_exp := Mux(exp_diff_sign, b_wrap.exponent, a_wrap.exponent)
  reg_sign := Mux(exp_diff_sign, b_wrap.sign, a_wrap.sign)

  // 输出结果
  io.out.bits.mant_shift := reg_mant_shift
  io.out.bits.b_larger := reg_b_larger
  io.out.bits.exp := reg_exp
  io.out.bits.manta := reg_manta
  io.out.bits.mantb := reg_mantb
  io.out.bits.sign := reg_sign
  io.out.bits.sub := reg_sub

  // 有效信号传递
  val valid = RegNext(io.a.valid && io.b.valid)
  io.out.valid := valid
}

class stage2Out(val expWidth: Int, val mantWidth: Int) extends Bundle {
  val manta_out = Output(UInt((mantWidth + 1).W))
  val mantb_out = Output(UInt((mantWidth + 1).W))
  val exp_out = Output(UInt(expWidth.W))
  val sign_out = Output(Bool())
  val sub_out = Output(Bool())
}

class FPAddStage2(val n: Int) extends Module {
  val (expWidth, mantWidth) = getExpMantWidths(n)

  val stage1Input = IO(Input(Valid(new stage1Out(expWidth, mantWidth))))

  val stage2Output = IO(Valid(new stage2Out(expWidth, mantWidth)))

  // in stage 2 we shift the appropriate mantissa by the amount
  // detected in stage 1

  val larger_mant = Mux(stage1Input.bits.b_larger, stage1Input.bits.mantb, stage1Input.bits.manta)
  val smaller_mant = Mux(stage1Input.bits.b_larger, stage1Input.bits.manta, stage1Input.bits.mantb)

  val shifted_mant =
    Mux(stage1Input.bits.mant_shift >= (mantWidth + 1).U, 0.U, smaller_mant >> stage1Input.bits.mant_shift)
  val reg_manta = RegNext(larger_mant)
  val reg_mantb = RegNext(shifted_mant)
  val reg_sign = RegNext(stage1Input.bits.sign)
  val reg_sub = RegNext(stage1Input.bits.sub)
  val reg_exp = RegNext(stage1Input.bits.exp)

  stage2Output.valid := RegNext(stage1Input.valid)

  stage2Output.bits.manta_out := reg_manta
  stage2Output.bits.mantb_out := reg_mantb
  stage2Output.bits.sign_out := reg_sign
  stage2Output.bits.sub_out := reg_sub
  stage2Output.bits.exp_out := reg_exp
}

class stage3Out(val expWidth: Int, val mantWidth: Int) extends Bundle {
  val mant_out = Output(UInt((mantWidth + 1).W))
  val sign_out = Output(Bool())
  val exp_out = Output(UInt(expWidth.W))
}

class FPAddStage3(val n: Int) extends Module {
  val (expWidth, mantWidth) = getExpMantWidths(n)

  val stage2Input = IO(Input(Valid(new stage2Out(expWidth, mantWidth))))

  val stage3Output = IO(Valid(new stage3Out(expWidth, mantWidth)))

  // in stage 3 we subtract or add the mantissas
  // we must also detect overflows and adjust sign/exponent appropriately

  val manta_ext = Cat(0.U(1.W), stage2Input.bits.manta_out)
  val mantb_ext = Cat(0.U(1.W), stage2Input.bits.mantb_out)
  val mant_sum = Mux(stage2Input.bits.sub_out, manta_ext -& mantb_ext, manta_ext +& mantb_ext)

  // here we drop the overflow bit
  val reg_mant = Reg(UInt((mantWidth + 1).W))
  val reg_sign = Reg(Bool())
  val reg_exp = Reg(UInt(expWidth.W))

  // this may happen if the operands were of opposite sign
  // but had the same exponent
  when(mant_sum(mantWidth + 1).asBool) {
    when(stage2Input.bits.sub_out) {
      reg_mant := -mant_sum(mantWidth, 0)
      reg_sign := !stage2Input.bits.sign_out
      reg_exp := stage2Input.bits.exp_out
    }.otherwise {
      // if the sum overflowed, we need to shift back by one
      // and increment the exponent
      reg_mant := mant_sum(mantWidth + 1, 1)
      reg_exp := stage2Input.bits.exp_out + 1.U
      reg_sign := stage2Input.bits.sign_out
    }
  }.otherwise {
    reg_mant := mant_sum(mantWidth, 0)
    reg_sign := stage2Input.bits.sign_out
    reg_exp := stage2Input.bits.exp_out
  }

  stage3Output.valid := RegNext(stage2Input.valid)
  stage3Output.bits.sign_out := reg_sign
  stage3Output.bits.exp_out := reg_exp
  stage3Output.bits.mant_out := reg_mant
}

class stage4Out(val expWidth: Int, val mantWidth: Int) extends Bundle {
  val mant_out = Output(UInt(mantWidth.W))
  val exp_out = Output(UInt(expWidth.W))
  val sign_out = Output(Bool())
}

class FPAddStage4(val n: Int) extends Module {
  val (expWidth, mantWidth) = getExpMantWidths(n)

  val stage3Input = IO(Input(Valid(new stage3Out(expWidth, mantWidth))))

  val stage4Output = IO(Valid(new stage4Out(expWidth, mantWidth)))

  // finally in stage 4 we normalize mantissa and exponent
  // we need to reverse the sum, since we want the find the most
  // significant 1 instead of the least significant 1

  val mant_out = stage3Input.bits.mant_out
  val norm_shift = PriorityEncoder(Reverse(mant_out))

  // if the mantissa sum is zero, result mantissa and exponent should be zero
  when(mant_out === 0.U) {
    stage4Output.bits.mant_out := RegNext(0.U)
    stage4Output.bits.exp_out := RegNext(0.U)
  }.otherwise {
    stage4Output.bits.mant_out := RegNext((mant_out << norm_shift)(mantWidth - 1, 0))
    stage4Output.bits.exp_out := RegNext(stage3Input.bits.exp_out - norm_shift)
  }
  stage4Output.bits.sign_out := RegNext(stage3Input.bits.sign_out)

  stage4Output.valid := RegNext(stage3Input.valid)

}

class FPAdd(val n: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(Valid(UInt(n.W)))
    val b = Input(Valid(UInt(n.W)))
    val res = Valid(UInt(n.W))
  })

  val (expWidth, mantWidth) = getExpMantWidths(n)

  val stage1 = Module(new FPAddStage1(n))

  stage1.io.a <> io.a
  stage1.io.b <> io.b

  val stage2 = Module(new FPAddStage2(n))

  stage2.stage1Input <> stage1.io.out

  val stage3 = Module(new FPAddStage3(n))

  stage3.stage2Input <> stage2.stage2Output

  val stage4 = Module(new FPAddStage4(n))

  stage4.stage3Input <> stage3.stage3Output

  io.res.bits := Cat(
    stage4.stage4Output.bits.sign_out,
    stage4.stage4Output.bits.exp_out,
    stage4.stage4Output.bits.mant_out
  )
  io.res.valid := stage4.stage4Output.valid

}

class FPAdd32 extends FPAdd(32) {}
class FPAdd64 extends FPAdd(64) {}
