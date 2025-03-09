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
        val minuendNums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val subtrahendNums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val quotientNums = minuendNums.zip(subtrahendNums).map { case (a, b) => a / b }

        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)

        fork {
          for (i <- 0 until nums) {
            val in1 = java.lang.Float.floatToRawIntBits(minuendNums(i)).toBinaryString
            val in2 = java.lang.Float.floatToRawIntBits(subtrahendNums(i)).toBinaryString

            dut.io.in1.bits.poke(BigInt(in1, 2).U)
            dut.io.in2.bits.poke(BigInt(in2, 2).U)
            dut.io.in1.valid.poke(true.B)
            dut.io.in2.valid.poke(true.B)
            dut.clock.step()
          }
          dut.io.in1.valid.poke(false.B)
          dut.io.in2.valid.poke(false.B)
          dut.clock.step()
        }.fork {
          // dut.clock.step(5)
          var clkCounter = 0
          while (clkCounter < nums) {
            if (dut.io.out.valid.peekBoolean()) {
              val calOut = java.lang.Float.intBitsToFloat(dut.io.out.peek().litValue.toInt)
              val missed = (calOut - quotientNums(clkCounter)) / quotientNums(clkCounter)
              println(
                s"${minuendNums(clkCounter)} / ${subtrahendNums(clkCounter)} = ${quotientNums(clkCounter)},\t calOut is: $calOut,\t missed is: $missed"
              )
              assert(missed.abs < math.pow(2, -22))
              clkCounter += 1
            }
            dut.clock.step()
          }
          dut.io.out.valid.expect(false.B)
        }.join()
      }
  }
}
