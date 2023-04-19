// See README.md for license details.

package tlb

import chisel3._
import chiseltest._
import chiseltest.formal._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._

class NutCoreFormalSpec extends AnyFlatSpec with Formal with ChiselScalatestTester {
  behavior of "TLBFormal"
  it should "pass" in {
    // verify
    verify(new TLB(), Seq(BoundedCheck(10), BtormcEngineAnnotation))
  }
}
