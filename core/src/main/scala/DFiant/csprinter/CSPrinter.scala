package DFiant
package csprinter

object CSPrinter {
  sealed trait Config extends DFiant.printer.Printer.Config {
    import io.AnsiColor._
    val showCustomTags : Boolean = true
    val showInits : Boolean = false
    val DELIM : String = "  "
    val maxAlignments : List[Int] = List(25, 25)
    val LIT : String = BLUE
    val STR : String = s"\u001B[38;5;34m$BOLD"
    val SC : String = s"$BLUE$BOLD"
    val DF : String = s"\u001B[38;5;92m$BOLD"
    val TP : String = "\u001B[38;5;94m"
    val CMT : String = "\u001B[38;5;247m"
  }
  object Config {
    implicit case object Default extends Config
    case object ShowInits extends Config {
      override val showInits: Boolean = true
    }
  }
}