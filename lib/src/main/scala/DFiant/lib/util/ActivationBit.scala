package DFiant.lib.util
import DFiant._

protected[util] object ActivationBit {
  @df object ActiveHigh extends DFOpaque.Of(Bit)
  @df object ActiveLow extends DFOpaque.Of(Bit)
  sealed trait Status
  object Status {
    object Active extends Status
    object Inactive extends Status
  }
}
