package fputil

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random
import fputil.Type.BFloat16

class FPAddTest extends AnyFlatSpec with ChiselScalatestTester {

  val nums = 20
  val randomRange = 30
  val annos = Seq(VerilatorBackendAnnotation)

  behavior.of("tester on fp32 adder")
  it should "fp32 adder" in {
    test(new FPAdd(32))
      .withAnnotations(annos) { dut =>
        val adder1Nums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val adder2Nums =
          Seq.tabulate(nums)(n =>
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        val resNums = adder1Nums.zip(adder2Nums).map { case (a, b) => a + b }
        println(s"resNums: ${resNums.mkString(", ")}")

        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)
        dut.io.a.valid.poke(false.B)
        dut.io.b.valid.poke(false.B)

        fork {
          for (i <- 0 until nums) {
            val in1 = java.lang.Float.floatToRawIntBits(adder1Nums(i)).toBinaryString
            val in2 = java.lang.Float.floatToRawIntBits(adder2Nums(i)).toBinaryString
            dut.io.a.bits.poke(BigInt(in1, 2).U)
            dut.io.b.bits.poke(BigInt(in2, 2).U)
            dut.io.a.valid.poke(true.B)
            dut.io.b.valid.poke(true.B)
            dut.clock.step()
          }
          dut.io.a.valid.poke(false.B)
          dut.io.b.valid.poke(false.B)
        }.fork {
          dut.io.res.valid.expect(false.B)
          dut.clock.step(3)
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

  behavior.of("tester on bf16 adder")
  it should "bf16 adder" in {
    test(new FPAddb16())
      .withAnnotations(annos) { dut =>
        val adder1Nums =
          Seq.tabulate(nums)(n =>
            BFloat16.fromFloat(
              (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
            )
          )
        val adder2Nums =
          Seq.tabulate(nums)(n =>
            BFloat16.fromFloat(
              (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
            )
          )
        val resNums = adder1Nums.zip(adder2Nums).map { case (a, b) => (a.toFloat + b.toFloat) }
        println(s"resNums: ${resNums.mkString(", ")}")

        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)
        dut.io.a.valid.poke(false.B)
        dut.io.b.valid.poke(false.B)

        fork {
          for (i <- 0 until nums) {
            // val in1 = java.lang.Float.floatToRawIntBits(adder1Nums(i)).toBinaryString
            // val in2 = java.lang.Float.floatToRawIntBits(adder2Nums(i)).toBinaryString
            val in1 = adder1Nums(i).toBinaryString
            val in2 = adder2Nums(i).toBinaryString
            dut.io.a.bits.poke(BigInt(in1, 2))
            dut.io.b.bits.poke(BigInt(in2, 2))
            dut.io.a.valid.poke(true.B)
            dut.io.b.valid.poke(true.B)
            dut.clock.step()
          }
          dut.io.a.valid.poke(false.B)
          dut.io.b.valid.poke(false.B)
        }.fork {
          dut.io.res.valid.expect(false.B)
          dut.clock.step(3)
          for (i <- 0 until nums) {
            dut.io.res.valid.expect(true.B)
            val calOut = new BFloat16(dut.io.res.bits.peekInt().toShort)
            val missed = math.abs(calOut.toFloat - resNums(i)) / math.abs(resNums(i))
            println(f"input = ${adder1Nums(i)} + ${adder2Nums(i)} = ${resNums(i)}\t output = $calOut")
            // println(f"calOut: ${calOut}%.4f\t resNums: ${resNums(i)}%.4f\t missed: ${missed}%.4f")
            // assert(missed < math.pow(2, -20))
            dut.clock.step()
          }
          dut.io.res.valid.expect(false.B)
        }.join()
      }
  }

}
