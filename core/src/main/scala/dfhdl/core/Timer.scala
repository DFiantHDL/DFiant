package dfhdl.core
import dfhdl.compiler.ir
import ir.{Time, Freq, Ratio}
import ir.Timer.Func.Op as FuncOp
import dfhdl.internals.*

sealed class Timer private (val irValue: ir.Timer | DFError) extends DFMember[ir.Timer]
object Timer:
  extension (timer: ir.Timer) private def asFE: Timer = new Timer(timer)
  type Period = Time | Freq
  extension (period: Period)
    def time = period match
      case t: Time => t
      case f: Freq => f.period

  extension (bd: BigDecimal.type)
    private def apply(arg: Int | Double): BigDecimal = arg match
      case i: Int    => BigDecimal(i)
      case d: Double => BigDecimal(d)

  object Periodic:
    def apply(trigger: Option[DFValOf[DFBit]], periodOpt: Option[Time])(using DFC): Timer =
      lazy val triggerRef: ir.Timer.TriggerRef = trigger match
        case Some(value) => value.asIR.refTW(timer)
        case None        => ir.DFRef.TwoWay.Empty
      lazy val timer: ir.Timer = ir.Timer
        .Periodic(
          triggerRef,
          periodOpt,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
      timer.asFE
  end Periodic
  object Func:
    def apply(source: Timer, op: FuncOp, arg: Time | Ratio)(using DFC): Timer =
      lazy val timer: ir.Timer = ir.Timer
        .Func(
          source.asIR.refTW(timer),
          op,
          arg,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
      timer.asFE

  object IsActive:
    def apply(timer: Timer)(using DFC): DFValOf[DFBool] =
      lazy val dfVal: ir.DFVal = ir.Timer
        .IsActive(
          timer.asIR.refTW(dfVal),
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
      dfVal.asValOf[DFBool]
end Timer
