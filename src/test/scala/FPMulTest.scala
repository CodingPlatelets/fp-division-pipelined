package fputil

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FPMulTest extends AnyFlatSpec with ChiselScalatestTester {

  val nums = 20
  val randomRange = 30
  val annos = Seq(VerilatorBackendAnnotation)

  behavior.of("tester on fp32 multiplication")
  it should "fp32 mul" in {
    test(new FPMult(32))
      .withAnnotations(annos) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)

        val multiplicandNums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val multiplierNums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val resNums = multiplierNums.zip(multiplicandNums).map { case (a, b) => a * b }

        for (i <- 0 until nums) {
          val in1 = java.lang.Float.floatToIntBits(multiplicandNums(i))
          val in2 = java.lang.Float.floatToIntBits(multiplierNums(i))
          val expectedOut = java.lang.Float.floatToIntBits(resNums(i))

          dut.io.a.poke(BigInt(in1 & 0xffffffffL).U)
          dut.io.b.poke(BigInt(in2 & 0xffffffffL).U)
          dut.clock.step(2)

          val calOut = java.lang.Float.intBitsToFloat(dut.io.res.peek().litValue.toInt)
          val missed = (calOut - resNums(i)) / resNums(i)
          // print(s"division is: ${minuendNums(i)} / ${subtrahendNums(i)} = ${quotientNums(i)}\n")
          // print(s"calOut is: ${calOut}\n")
          // print(s"missed is: ${missed}\n")
          // print(s"\n")
          assert(missed.abs < math.pow(2, -22))
        }
      }
  }
}
