package fputil.example

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random
import fputil.Type.BFloat16

class VecMulTest extends AnyFlatSpec with ChiselScalatestTester {

  val annos = Seq(VerilatorBackendAnnotation)

  val size = 32
  behavior.of("test example vec mul")
  it should "vec bf mull" in {
    test(new VecMul(32, 16)).withAnnotations(annos) { dut =>
      val mul1 = Seq.tabulate(size)(n =>
        BFloat16.fromFloat(
          10 * Random.nextFloat()
        )
      )

      val mul2 = Seq.tabulate(size)(n =>
        BFloat16.fromFloat(
          10 * Random.nextFloat()
        )
      )

      val res = mul1.zip(mul2).map { case (a, b) => a * b }.reduce(_ + _)
      // println(mul1)
      // println(mul2)
      println(res)

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)

      // fork
      fork {
        for (i <- 0 until size) {
          val in1 = mul1(i).toBinaryString
          val in2 = mul2(i).toBinaryString
          dut.io.in1.bits(i).poke(BigInt(in1, 2))
          dut.io.in2.bits(i).poke(BigInt(in2, 2))
        }
        dut.io.in1.valid.poke(true.B)
        dut.io.in2.valid.poke(true.B)
        dut.clock.step()
      }.fork {
        // dut.io.in1.valid.poke(false.B)
        // dut.io.in2.valid.poke(false.B)
        while (!dut.io.out.valid.peekBoolean()) {
          dut.clock.step()
        }
        val calOut = new BFloat16(dut.io.out.bits.peekInt().toShort)
        println(calOut)
      }.join()
    }
  }
}
