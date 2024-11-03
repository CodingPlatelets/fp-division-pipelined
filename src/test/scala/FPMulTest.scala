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
        val multiplicandNums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val multiplierNums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val resNums = multiplierNums.zip(multiplicandNums).map { case (a, b) => a * b }

        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)

        fork {
          for (i <- 0 until nums) {
            val in1 = java.lang.Float.floatToRawIntBits(multiplicandNums(i)).toBinaryString
            val in2 = java.lang.Float.floatToRawIntBits(multiplierNums(i)).toBinaryString
            val expectedOut = java.lang.Float.floatToRawIntBits(resNums(i)).toBinaryString

            dut.io.a.bits.poke(BigInt(in1, 2).U)
            dut.io.b.bits.poke(BigInt(in2, 2).U)
            dut.io.a.valid.poke(true.B)
            dut.io.b.valid.poke(true.B)
            dut.clock.step()
          }
          dut.io.a.valid.poke(false.B)
          dut.io.b.valid.poke(false.B)
          dut.clock.step()
        }.fork {
          dut.clock.step(1)
          for (i <- 0 until nums) {
            dut.io.res.valid.expect(true.B)
            val calOut = java.lang.Float.intBitsToFloat(dut.io.res.bits.peekInt().toInt)
            val missed = (calOut - resNums(i)) / resNums(i)
            println(f"input = ${multiplicandNums(i)} * ${multiplierNums(i)} = ${resNums(i)}\t output = $calOut")
            // assert(missed.abs < math.pow(2, -22))
            dut.clock.step()
          }
          dut.io.res.valid.expect(false.B)
        }.join()
      }
  }
}
