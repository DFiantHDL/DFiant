package ZFiant
package compiler.backend.vhdl

private object Architecture {
  def apply(name : String, entityName : String, declarations : List[String], statements : List[String])(
    implicit printer : Printer
  )  : String = {
    import printer.config._
    import formatter._
    val declarationStr = if (declarations.isEmpty) "" else declarations.mkString("\n","\n","").delim()
    s"""$KW architecture $name $KW of $entityName $KW is$declarationStr
       |$KW begin
       |${statements.mkString("\n").delim()}
       |$KW end $name;""".stripMargin
  }
}
