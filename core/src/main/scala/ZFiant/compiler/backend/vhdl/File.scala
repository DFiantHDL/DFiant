package ZFiant
package compiler.backend.vhdl

private object File {
  def apply(packageName : String, entity: String, architecture: String)(implicit printer: Printer) : String = {
    s"${Library(packageName)}$entity\n\n$architecture"
  }
}

//////////////////////////////////////////////////////////////////////////////////
// Library
//////////////////////////////////////////////////////////////////////////////////
private object Library {
  def apply(packageName : String)(implicit printer: Printer) : String = {
    import printer.config._

    s"""$KW library $TP ieee;
       |$KW use $TP ieee.$TP std_logic_1164.$KW all;
       |$KW use $TP ieee.$TP numeric_std.$KW all;
       |$KW use $TP work.$packageName.$KW all;
       |""".stripMargin
  }
}
private object SimLibrary {
  def apply(inSimulation : Boolean)(implicit printer: Printer) : String =
    if (inSimulation)
      s"""library std;
         |use std.env.all;
         |""".stripMargin
    else ""
}
//////////////////////////////////////////////////////////////////////////////////
