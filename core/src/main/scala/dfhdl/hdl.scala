package dfhdl
object hdl:
  class dsn extends scala.annotation.StaticAnnotation
  export core.DFBoolOrBit.Token.Ops.*
  export core.DFBoolOrBit.Val.Ops.*
  export core.width
  type Timer = core.Timer
  val Timer = core.Timer

  lazy val Bit = core.DFBit
  type Bit = core.DFBit
end hdl

export hdl.*
