package fputil

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FMATest extends AnyFlatSpec with ChiselScalatestTester {

  val nums = 20
  val randomRange = 30
  val annos = Seq(VerilatorBackendAnnotation)

  behavior.of("tester on fp32 fma")
  it should "fp32 fma" in {
    test(new FMA(32))
      .withAnnotations(annos) { dut =>
        val aNums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val bNums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val cNums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val resNums = aNums.zip(bNums).zip(cNums).map { case ((a, b), c) => a * b + c }
        println(s"resNums: ${resNums.mkString(", ")}")

        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)
        dut.io.a.valid.poke(false.B)
        dut.io.b.valid.poke(false.B)

        fork {
          for (i <- 0 until nums) {
            val in1 = java.lang.Float.floatToRawIntBits(aNums(i)).toBinaryString
            val in2 = java.lang.Float.floatToRawIntBits(bNums(i)).toBinaryString
            val in3 = java.lang.Float.floatToRawIntBits(cNums(i)).toBinaryString
            dut.io.a.bits.poke(BigInt(in1, 2).U)
            dut.io.b.bits.poke(BigInt(in2, 2).U)
            dut.io.c.bits.poke(BigInt(in3, 2).U)
            dut.io.a.valid.poke(true.B)
            dut.io.b.valid.poke(true.B)
            dut.io.c.valid.poke(true.B)
            dut.clock.step()
          }
          dut.io.a.valid.poke(false.B)
          dut.io.b.valid.poke(false.B)
          dut.io.c.valid.poke(false.B)
        }.fork {
          dut.io.res.valid.expect(false.B)
          dut.clock.step(4)
          for (i <- 0 until nums) {
            dut.io.res.valid.expect(true.B)
            val calOut = java.lang.Float.intBitsToFloat(dut.io.res.bits.peekInt().toInt)
            val missed = math.abs(calOut - resNums(i)) / math.abs(resNums(i))
            println(f"calOut: ${calOut}%.4f\t resNums: ${resNums(i)}%.4f\t missed: ${missed}%.4f")
            assert(missed < math.pow(2, -20))
            dut.clock.step()
          }
          dut.io.res.valid.expect(false.B)
        }.join()
      }

  }

}
