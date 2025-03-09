package fputil

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random
import fputil.Type.BFloat16

class FPMulTest extends AnyFlatSpec with ChiselScalatestTester {

  val nums = 20
  val randomRange = 30
  val annos = Seq(VerilatorBackendAnnotation)

  // behavior.of("tester on fp32 multiplication")
  // it should "fp32 mul" in {
  //   test(new FPMult(32))
  //     .withAnnotations(annos) { dut =>
  //       val multiplicandNums =
  //         Seq.tabulate(nums)(n =>
  //           (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
  //         )
  //       val multiplierNums =
  //         Seq.tabulate(nums)(n =>
  //           (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
  //         )
  //       val resNums = multiplierNums.zip(multiplicandNums).map { case (a, b) => a * b }

  //       dut.reset.poke(true.B)
  //       dut.clock.step(1)
  //       dut.reset.poke(false.B)

  //       fork {
  //         for (i <- 0 until nums) {
  //           val in1 = java.lang.Float.floatToRawIntBits(multiplicandNums(i)).toBinaryString
  //           val in2 = java.lang.Float.floatToRawIntBits(multiplierNums(i)).toBinaryString
  //           val expectedOut = java.lang.Float.floatToRawIntBits(resNums(i)).toBinaryString

  //           dut.io.a.bits.poke(BigInt(in1, 2).U)
  //           dut.io.b.bits.poke(BigInt(in2, 2).U)
  //           dut.io.a.valid.poke(true.B)
  //           dut.io.b.valid.poke(true.B)
  //           dut.clock.step()
  //         }
  //         dut.io.a.valid.poke(false.B)
  //         dut.io.b.valid.poke(false.B)
  //         dut.clock.step()
  //       }.fork {
  //         dut.clock.step(1)
  //         for (i <- 0 until nums) {
  //           dut.io.res.valid.expect(true.B)
  //           val calOut = java.lang.Float.intBitsToFloat(dut.io.res.bits.peekInt().toInt)
  //           val missed = (calOut - resNums(i)) / resNums(i)
  //           println(f"input = ${multiplicandNums(i)} * ${multiplierNums(i)} = ${resNums(i)}\t output = $calOut")
  //           // assert(missed.abs < math.pow(2, -22))
  //           dut.clock.step()
  //         }
  //         dut.io.res.valid.expect(false.B)
  //       }.join()
  //     }
  // }

  behavior.of("tester on bf 16 multiplication")
  it should "bf16 mul" in {
    test(new FPMultb16).withAnnotations(annos) { dut =>
      val multiplicandNums =
        Seq.tabulate(nums)(n =>
          BFloat16.fromFloat(
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        )
      val multiplierNums =
        Seq.tabulate(nums)(n =>
          BFloat16.fromFloat(
            (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(randomRange) + 1) * Random.nextFloat()
          )
        )
      val resNums = multiplierNums.zip(multiplicandNums).map { case (a, b) => (a.toFloat * b.toFloat) }

      println(multiplicandNums)
      println(multiplierNums)
      println(resNums)

      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)

      // var tmp_16 = multiplicandNums(i)
      val tmp_16 = BFloat16.fromFloat(-1.2f)
      println(tmp_16.toBinaryString)
      println(java.lang.Float.floatToIntBits(tmp_16.toFloat).toBinaryString.dropRight(16))
      fork {
        for (i <- 0 until nums) {
          val in1 = multiplicandNums(i).toBinaryString
          val in2 = multiplierNums(i).toBinaryString
          // val expectedOut = java.lang.Float.floatToRawIntBits(resNums(i)).toBinaryString.slice(0, 17)

          dut.io.a.bits.poke(BigInt(in1, 2))
          dut.io.b.bits.poke(BigInt(in2, 2))
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
          val calOut = new BFloat16(dut.io.res.bits.peekInt().toShort)
          val missed = (calOut.toFloat - resNums(i)) / resNums(i)
          println(f"input = ${multiplicandNums(i)} * ${multiplierNums(i)} = ${resNums(i)}\t output = $calOut")
          // assert(missed.abs < math.pow(2, -22))
          dut.clock.step()
        }
        dut.io.res.valid.expect(false.B)
      }.join()
    }
  }
}
