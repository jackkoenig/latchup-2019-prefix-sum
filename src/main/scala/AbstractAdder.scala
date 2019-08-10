
import chisel3._

import simple._

class AbstractAdder(val width: Int, prefixSum: PrefixSum) extends Module {
  require(width > 0)
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val z = Output(UInt((width+1).W))
  })

  // Split up bit vectors into individual bits
  val as = io.a.toBools
  val bs = io.b.toBools

  // P = Ai xor Bi
  // G = Ai and Bi
  val pairs = as.zip(bs).map { case (a, b) => (a ^ b, a && b) }

  // At each stage of prefix sum tree
  // P = Pi and Pi_prev
  // G = (Pi and Gi_prev) or Gi
  val pgs = prefixSum(pairs) {
    case ((pp, gp), (pi, gi)) => (pi && pp, (pi && gp) || gi)
  }

  // Carries are Generates from end of prefix sum
  val cs = false.B +: pgs.map(_._2) // Include carry-in of 0
  // Sum requires propogate bits from first step
  val ps = pairs.map(_._1) :+ false.B // Include P for overflow
  // Si = Pi xor Ci-1
  val sum = ps.zip(cs).map { case (p, c) => p ^ c }

  // Recombine bits into bitvector
  io.z := VecInit(sum).asUInt
}

class RippleCarryAdder(width: Int) extends AbstractAdder(width, RipplePrefixSum)
class KoggeStoneAdder2(width: Int) extends AbstractAdder(width, DensePrefixSum)
class BrentKungAdder(width: Int) extends AbstractAdder(width, SparsePrefixSum)

object AbstractAdderMain extends App {
  //println(chisel3.Driver.emitVerilog(new KoggeStoneAdder(2)))
  val size = 4
  chisel3.Driver.execute(args, () => new AbstractAdder(size, SparsePrefixSum))
}
