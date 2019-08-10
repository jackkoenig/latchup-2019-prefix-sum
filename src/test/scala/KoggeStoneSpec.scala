
import chisel3._
import chisel3.util._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}

class KoggeStoneTester(dut: KoggeStoneAdder) extends PeekPokeTester(dut) {
  val max = math.pow(2, dut.width).toInt // Inclusive, using until below
  for (i <- 0 until max) {
    for (j <- 0 until max) {
      poke(dut.io.a, i)
      poke(dut.io.b, j)
      expect(dut.io.z, i + j)
      step(1)
    }
  }
}


class KoggeStoneSpec extends FlatSpec with Matchers {
  behavior of "Kogge-Stone Adder"

  it should "work" in {
    chisel3.iotesters.Driver(() => new KoggeStoneAdder(2))(dut =>
      new KoggeStoneTester(dut)
    ) should be (true)
  }
}
