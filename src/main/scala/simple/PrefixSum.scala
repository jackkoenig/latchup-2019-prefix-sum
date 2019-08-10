// See LICENSE for license details.

package simple

trait PrefixSum {
  // out[0] = summands[0]
  // out[1] = summands[0] + summands[1]
  // out[2] = summands[0] + summands[1] + summands[2]
  // ...
  // where + is your associative operator (reflexivity not required)
  def apply[T](summands: Seq[T])(associativeOp: (T, T) => T): Vector[T]
}

// N-1 area, N-1 depth
object RipplePrefixSum extends PrefixSum {
  def apply[T](summands: Seq[T])(associativeOp: (T, T) => T): Vector[T] = {
    def helper(offset: Int, x: Vector[T]): Vector[T] = {
      if (offset >= x.size) {
        x
      } else {
        val layer = Vector.tabulate(x.size) { i =>
          if (i != offset) {
            x(i)
          } else {
            associativeOp(x(i-1), x(i))
          }
        }
        helper(offset+1, layer)
      }
    }
    helper(1, summands.toVector)
  }
}

// O(NlogN) area, logN depth
object DensePrefixSum extends PrefixSum {
  def apply[T](summands: Seq[T])(associativeOp: (T, T) => T): Vector[T] = {
    def helper(offset: Int, x: Vector[T]): Vector[T] = {
      if (offset >= x.size) {
        x
      } else {
        val layer = Vector.tabulate(x.size) { i =>
          if (i < offset) {
            x(i)
          } else {
            associativeOp(x(i-offset), x(i))
          }
        }
        helper(offset << 1, layer)
      }
    }
    helper(1, summands.toVector)
  }
}

// 2N area, 2logN depth
object SparsePrefixSum extends PrefixSum {
  def apply[T](summands: Seq[T])(associativeOp: (T, T) => T): Vector[T] = {
    def contract(offset: Int, x: Vector[T]): Vector[T] = {
      val double = offset << 1
      val offset1 = offset - 1
      if (offset <= 0) {
        x
      } else if (double+offset1 >= x.size) {
        contract(offset >> 1, x)
      } else {
        val layer = Vector.tabulate(x.size) { i =>
          if (i % double == offset1 && i >= offset) {
            associativeOp(x(i-offset), x(i))
          } else {
            x(i)
          }
        }
        contract(offset >> 1, layer)
      }
    }
    def expand(offset: Int, x: Vector[T]): Vector[T] = {
      val double = offset << 1
      val double1 = double - 1
      if (double1 >= x.size) {
        contract(offset >> 1, x)
      } else {
        val layer = Vector.tabulate(x.size) { i =>
          if (i % double == double1) {
            associativeOp(x(i-offset), x(i))
          } else {
            x(i)
          }
        }
        expand(double, layer)
      }
    }
    expand(1, summands.toVector)
  }
}

