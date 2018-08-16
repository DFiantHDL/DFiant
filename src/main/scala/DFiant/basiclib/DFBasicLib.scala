package DFiant.basiclib

import DFiant._
import DFComponent.Implementation
import singleton.twoface._

trait DFBasicLib {

  val DFUIntOps : DFBasicLib.DFUIntOps
  val DFBitsOps : DFBasicLib.DFBitsOps
  val DFBoolOps : DFBasicLib.DFBoolOps

}


object DFBasicLib {
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFUInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFUIntOps {
    import DFiant.basiclib.DFUIntOps._
    implicit val `Comp+`  : Implementation[`Comp+`]
    implicit val `Comp-`  : Implementation[`Comp-`]
    implicit val `Comp*`  : Implementation[`Comp*`]

    implicit val `Comp==` : Implementation[`Comp==`]
    implicit val `Comp!=` : Implementation[`Comp!=`]
    implicit val `Comp<`  : Implementation[`Comp<`]
    implicit val `Comp>`  : Implementation[`Comp>`]
    implicit val `Comp<=` : Implementation[`Comp<=`]
    implicit val `Comp>=` : Implementation[`Comp>=`]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFBits
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFBitsOps {
    import DFiant.basiclib.DFBitsOps._
    implicit val `Comp|`  : Implementation[`Comp|`]
    implicit val `Comp&`  : Implementation[`Comp&`]
    implicit val `Comp^`  : Implementation[`Comp^`]

    implicit val `Comp==` : Implementation[`Comp==`]
    implicit val `Comp!=` : Implementation[`Comp!=`]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFBool
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFBoolOps {
    import DFiant.basiclib.DFBoolOps._
    implicit val `Comp||` : Implementation[`Comp||`]
    implicit val `Comp&&` : Implementation[`Comp&&`]
    implicit val `Comp==` : Implementation[`Comp==`]
    implicit val `Comp!=` : Implementation[`Comp!=`]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}