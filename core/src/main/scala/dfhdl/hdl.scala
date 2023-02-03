package dfhdl
object hdl:
  class dsn extends scala.annotation.StaticAnnotation
  export core.DFType.Ops.*
  export core.DFToken.Ops.*
  export core.DFBoolOrBit.Token.Ops.*
  export core.DFBoolOrBit.Val.Ops.*
  export core.{width, dfType}
  export core.Timer.Ops.*
  export core.Wait.Ops.*
  type Timer = core.Timer
  val Timer = core.Timer
  export core.dfc
  export internals.Inlined

  lazy val Bit = core.DFBit
  type Bit = core.DFBit
end hdl

export hdl.*
