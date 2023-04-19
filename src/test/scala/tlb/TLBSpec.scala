// See README.md for license details.

package tlb

import chisel3._
import chiseltest._
import chiseltest.formal._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._

class TLBSpec extends AnyFlatSpec with Formal with ChiselScalatestTester {
  behavior of "TLBFormal"
  it should "pass" in {
    // verify
    val dtlbconfig = TLBConfig(name = "dtlb", totalEntry = 64)
    // verify(new EmbeddedTLB_test(), Seq(BoundedCheck(100), BtormcEngineAnnotation))
    verify(new EmbeddedTLB()(dtlbconfig), Seq(BoundedCheck(100), BtormcEngineAnnotation))
  }
}
