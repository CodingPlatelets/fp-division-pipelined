package fputil

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FPDivTest extends AnyFlatSpec with ChiselScalatestTester {

  val nums = 20
  val randomRange = 30
  val annos = Seq(VerilatorBackendAnnotation)

  behavior.of("tester on fp32 division")
  it should "fp32 division" in {
    test(new FPDiv(32))
      .withAnnotations(annos) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)

        val minuendNums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val subtrahendNums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val quotientNums = minuendNums.zip(subtrahendNums).map { case (a, b) => a / b }

        for (i <- 0 until nums) {
          val in1 = java.lang.Float.floatToIntBits(minuendNums(i))
          val in2 = java.lang.Float.floatToIntBits(subtrahendNums(i))

          dut.io.in1.poke(BigInt(in1 & 0xffffffffL).U)
          dut.io.in2.poke(BigInt(in2 & 0xffffffffL).U)
          dut.clock.step(5)

          val calOut = java.lang.Float.intBitsToFloat(dut.io.out.peek().litValue.toInt)
          val missed = (calOut - quotientNums(i)) / quotientNums(i)
          // print(s"division is: ${minuendNums(i)} / ${subtrahendNums(i)} = ${quotientNums(i)}\n")
          // print(s"calOut is: ${calOut}\n")
          // print(s"missed is: ${missed}\n")
          // print(s"\n")
          assert(missed.abs < math.pow(2, -22))
        }
      }
  }
}
