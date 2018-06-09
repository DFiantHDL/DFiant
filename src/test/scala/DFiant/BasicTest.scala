package DFiant

object BasicTest extends App {

  trait MemeDesign extends DFDesign {
    type W = 8
    val meme = new DFDesign {
      val a : DFUInt[W] <> IN = OPEN
      def implementation(): Unit = {}
    }
    new DFDesign {
      val a : DFUInt[W] <> IN = OPEN
      def implementation(): Unit = {}
    }
  }

  trait IODesign extends DFDesign {
    type W = 8
    val i : DFUInt[W] <> IN
    val o : DFUInt[W] <> OUT
    def implementation() : Unit = {
      o := i
    }
  }

  trait MyDesign extends DFDesign {
    type W = 8
    val a_in : DFUInt[W] <> IN = TOP
    val b_in : DFUInt[W] <> IN = TOP
    val c_out : DFUInt[W] <> OUT = TOP
    val d_out : DFUInt[W] <> OUT = TOP

    val oron = DFUInt[8]
    val io1 = new DFDesign {
      val i : DFUInt[W] <> IN = a_in
      val o : DFUInt[W] <> OUT = c_out
      def implementation(): Unit = {
        o := i
      }
    }

    val io2 = new IODesign {
      val i = b_in
      val o = d_out
    }

    def implementation(): Unit = {
    }
    //c_out := a_in + b_in
  }

  import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
  val myDesign = new MyDesign {}
  println(myDesign.portsOut)
//  myDesign.meme.keep
//  myDesign.wakeLazyAlmanac
//  myDesign.protAlmanac.printEntrees()
//  println(myDesign.meme.getName)
//  myDesign.keep
//  println(myDesign.components)

  myDesign.printInfo()

//  myDesign.compileToVHDL("myDesignTest")
}
