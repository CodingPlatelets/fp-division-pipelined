package fputil

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FPAddTest extends AnyFlatSpec with ChiselScalatestTester {

  val nums = 20
  val randomRange = 30
  val annos = Seq(VerilatorBackendAnnotation)

  behavior.of("tester on fp32 adder")
  it should "fp32 adder" in {
    test(new FPAdd(32))
      .withAnnotations(annos) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)

        val adder1Nums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val adder2Nums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val resNums = adder1Nums.zip(adder2Nums).map { case (a, b) => a + b }

        for (i <- 0 until nums) {
          val in1 = java.lang.Float.floatToIntBits(adder1Nums(i))
          val in2 = java.lang.Float.floatToIntBits(adder2Nums(i))

          dut.io.a.poke(BigInt(in1 & 0xffffffffL).U)
          dut.io.b.poke(BigInt(in2 & 0xffffffffL).U)
          dut.clock.step(4)

          val calOut = java.lang.Float.intBitsToFloat(dut.io.res.peek().litValue.toInt)
          val missed = math.abs(calOut - resNums(i)) / resNums(i)
          // print(s"adder is: ${adder1Nums(i)} + ${adder2Nums(i)} = ${resNums(i)}\n")
          // print(s"calOut is: ${calOut}\n")
          // print(s"missed is: ${missed}\n")
          // print(s"\n")
          assert(missed.abs < math.pow(2, -20))
        }
      }

  }

}
