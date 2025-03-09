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
        fork {
          for (i <- 0 until nums) {
            val in1 = java.lang.Float.floatToRawIntBits(adder1Nums(i)).toBinaryString
            val in2 = java.lang.Float.floatToRawIntBits(adder2Nums(i)).toBinaryString
            dut.io.a.poke(BigInt(in1, 2).U)
            dut.io.b.poke(BigInt(in2, 2).U)
            dut.clock.step()
          }
        }.fork {
          dut.clock.step(3)
          for (i <- 0 until nums) {
            val calOut = java.lang.Float.intBitsToFloat(dut.io.res.peekInt().toInt)
            val missed = math.abs(calOut - resNums(i)) / math.abs(resNums(i))
            println(f"calOut: ${calOut}%.4f\t resNums: ${resNums(i)}%.4f\t missed: ${missed}%.4f")
            assert(missed < math.pow(2, -20))
            dut.clock.step()
          }
        }.join()
      }
  }

}
