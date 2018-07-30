package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._


trait IODesignConn1 extends DFDesign {
  val i = DFUInt(8) <> IN init(1,2)
  val o = DFUInt(8) <> OUT
  o <> i
}

trait IODesignIf extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val b = DFBool() <> IN
  val myIf = ifdf (b) {
    val myIf2 = ifdf (b) {
      o := i
    }.elseifdf(b) {
      o := i
    }
  }.elsedf {
    o := i
  }
}

trait IODesignConn2 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  i <> o
}

trait IODesignConn3 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val aa = i + 1
  o <> aa
}

trait ContainerConn1 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io = new IODesignConn1 {}
  i    <> io.i //Connecting between owner input and child input
  io.o <> o    //Connecting between child output and owner output
}

trait ContainerConnLoop extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io = new IODesignConn1 {}
  io.i <> io.o
  o <> io.o
}

trait ContainerConn3 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io1 = new IODesignConn1 {}
  val io2 = new IODesignConn1 {}
  i     <> io1.i //Connecting between owner input and child input
  io1.o <> io2.i //Connecting between siblings (output <> input)
  io2.o <> o     //Connecting between child output and owner output
}

trait ContainerConn4 extends DFDesign {
  val i = DFUInt(7) <> IN
  val o = DFUInt(8) <> OUT
  val ob = DFBool() <> OUT
  val io = new IODesignConn1 {}
  i <> io.i
  io.o <> o
  true <> ob
}

//trait IODesign extends DFDesign {
//  val i = DFUInt(8) <> IN
//  val o = DFUInt(8) <> OUT
//  o := i
//}
//
//trait IODesign1 extends DFDesign {
//  val i = DFUInt(8) <> IN
//  val o = DFUInt(8) <> OUT
//  val tmp = DFUInt(8)
//  tmp := i
//  o := tmp
//}
//
//trait IODesign2 extends DFDesign {
//  val i1 = DFUInt(8) <> IN
//  val o1 = DFUInt(8) <> OUT
//  val i2 = DFUInt(8) <> IN
//  val o2 = DFUInt(8) <> OUT
//  o1 := i1
//  o2 := i2
//}
//
//



object BasicTest extends App {
  import psuedoVendor.family.device._
  val top_ioDesignConn1 = new IODesignConn1 {}
  val top_ioDesignConn3 = new IODesignConn3 {}
  val top_containerConn1 = new ContainerConn1 {}
  val top_containerConn3 = new ContainerConn3 {}
  val top_containerConn4 = new ContainerConn4 {}
  val top_ioDesignIf = new IODesignIf {}
  println(top_ioDesignIf.codeString)

}













//
//object BasicTest extends App {
//
//  trait MemeDesign extends DFDesign {
//    type W = 8
//    val meme = new DFDesign {
//      val a : DFUInt[W] <> IN = OPEN
//    }
//    new DFDesign {
//      val a : DFUInt[W] <> IN = OPEN
//    }
//  }
//
//  trait IODesign extends DFDesign {
//    type W = 8
//    val i : DFUInt[W] <> IN
//    val o : DFUInt[W] <> OUT
//    val tmp = DFUInt[W]
//    tmp := i
//    o := tmp
//  }
//
//  trait IOComp extends DFComponent[IOComp] {
//    type W = 8
//    val i : DFUInt[W] <> IN
//    val o : DFUInt[W] <> OUT
//  }
//
//  object IOComp {
//    implicit def ev : DFComponent.Implementation[IOComp] = ifc => {
//      import ifc._
//      val tmp = DFUInt[W]
//      val rt = new RTIOComp {
//        val i = ifc.i
//        val o = tmp
//      }
//
//      o := tmp
//    }
//
//  }
//
//  trait RTIOComp extends RTComponent {
//    type W = 8
//    val i : DFUInt[W] <> IN
//    val o : DFUInt[W] <> OUT
//  }
//
//  trait MyDesign extends DFDesign {
//    type W = 8
//    val a_in : DFUInt[W] <> IN = TOP
//    val b_in : DFUInt[W] <> IN = TOP
//    val c_in : DFUInt[W] <> IN = TOP
//    val d_in : DFUInt[W] <> IN = TOP
//    val e_in : DFUInt[W] <> IN = TOP
//    val a_out : DFUInt[W] <> OUT = TOP
//    val b_out : DFUInt[W] <> OUT = TOP
//    val c_out : DFUInt[W] <> OUT = TOP
//    val d_out : DFUInt[W] <> OUT = TOP
//    val e_out : DFUInt[W] <> OUT = TOP
//
//    val a_io = new DFDesign {
//      a_io =>
//      val i : DFUInt[W] <> IN = a_in
//      val o : DFUInt[W] <> OUT = a_out
//      val internal = new DFDesign {
//        val i : DFUInt[W] <> IN = a_io.i
//        val o : DFUInt[W] <> OUT = a_io.o
//        val tmp = DFUInt[W]
//
//        tmp := i.prev()
//        o := tmp
//      }
//    }
//
//    val b_io = new IODesign {
//      val i = b_in
//      val o = b_out
//    }
//
//    val c_io = new IOComp {
//      val i = c_in
//      val o = c_out
//    }
//
//    val d_io = new RTIOComp {
//      val i = d_in
//      val o = d_out
//    }
//
//    e_out := e_in + e_in
//  }
//
//  import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
//  val myDesign = new MyDesign {}
//  println(myDesign.portsOut)
////  myDesign.meme.keep
////  myDesign.wakeLazyAlmanac
////  myDesign.protAlmanac.printEntrees()
////  println(myDesign.meme.name)
////  myDesign.keep
////  println(myDesign.components)
//
//  myDesign.printInfo()
//  println(myDesign.a_io.portNodes)
//
////  myDesign.compileToVHDL("myDesignTest")
//}
