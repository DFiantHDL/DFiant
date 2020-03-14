package ZFiant
package compiler.backend.vhdl

private object Type {
  def apply(member : DFAny)(implicit printer : Printer) : String = {
    import printer.config._
    member match {
      case DFBits(width) => s"std_logic_vector(${width-1} $KW downto 0)"
      case DFUInt(width) => s"unsigned(${width-1} $KW downto 0)"
      case DFSInt(width) => s"signed(${width-1} $KW downto 0)"
      case DFEnum(enumType) => enumType.name
      case DFBit() => s"std_logic"
      case DFBool() => s"boolean"
      case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${member.getFullName} has type ${member.typeName}")
    }
  }
}
