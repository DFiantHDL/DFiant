package DFiant
package compiler.backend.vhdl

private object File {
  def apply(packageName : String, entity: String, architecture: String)(implicit printer: Printer, revision: VHDLRevision) : String = {
    import printer.config.formatter._
    s"""
       |${Library(packageName)}
       |${SimLibrary()}
       |$EMPTY
       |$entity
       |$EMPTY
       |$architecture""".stripMargin.formatted
  }
}

//////////////////////////////////////////////////////////////////////////////////
// Library
//////////////////////////////////////////////////////////////////////////////////
private object Library {
  def apply(packageName : String)(implicit printer: Printer, revision: VHDLRevision) : String = {
    import printer.config._
    s"""$KW library $TP ieee;
       |$KW use $TP ieee.$TP std_logic_1164.$KW all;
       |$KW use $TP ieee.$TP numeric_std.$KW all;
       |$KW use $TP work.$packageName.$KW all;
       |""".stripMargin
  }
}
private object SimLibrary {
  def apply()(implicit printer: Printer, revision: VHDLRevision) : String = {
    import printer.config._
    import formatter._
    if (printer.inSimulation) revision match {
      case VHDLRevision.VHDL1993 => ""
      case VHDLRevision.VHDL2008 =>
        s"""$EMPTY
           |$KW library $TP std;
           |$KW use $TP std.$TP env.$KW all;
           |""".stripMargin
    } else ""
  }
}
//////////////////////////////////////////////////////////////////////////////////
