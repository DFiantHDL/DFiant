package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.Op as FuncOp

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*
import DFOpaque.Abstract as DFOpaqueA
import dfhdl.compiler.ir.MemberGetSet
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import scala.annotation.tailrec

import scala.reflect.ClassTag
class DFVal[+T <: DFTypeAny, +M <: ModifierAny](val irValue: ir.DFVal | DFError)
    extends // AnyVal with
    DFMember[ir.DFVal]
    with Selectable:

  def selectDynamic(name: String)(using DFC): Any = trydf {
    val ir.DFStruct(structName, fieldMap) = this.asIR.dfType: @unchecked
    val dfType = fieldMap(name)
    DFVal.Alias
      .SelectField(this, name)
      .asIR
      .asVal[DFTypeAny, ModifierAny]
  }

  transparent inline def ==[R](
      inline that: R
  )(using DFC): DFBool <> VAL = ???
  transparent inline def !=[R](
      inline that: R
  )(using DFC): DFBool <> VAL = ???
end DFVal

type DFValAny = DFVal[DFTypeAny, ModifierAny]
type DFVarAny = DFVal[DFTypeAny, Modifier[Modifier.Assignable, Modifier.Connectable, Any]]
type DFValOf[+T <: DFTypeAny] = DFVal[T, ModifierAny]
type DFVarOf[+T <: DFTypeAny] = DFVal[T, Modifier[Modifier.Assignable, Any, Any]]
type DFPortOf[+T <: DFTypeAny] = DFVal[T, Modifier.Port]

sealed trait TOKEN
type <>[T <: DFType.Supported | Int, M] = T match
  case DFType.Supported =>
    M match
      case VAL   => DFValOf[DFType.Of[T]]
      case TOKEN => DFToken[DFType.Of[T]]
  // Int is also special cased by the compiler plugin
  case Int => DFVector.ComposedModifier[T, M]

type X[T <: DFType.Supported, M] = M match
  case DFVector.ComposedModifier[d, m] => <>[DFVector[DFType.Of[T], Tuple1[d]], m]
  case Int                             => DFVector[DFType.Of[T], Tuple1[M]]
type JUSTVAL[T <: DFType.Supported] = <>[T, VAL]

extension (dfVal: ir.DFVal)
  inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] =
    DFVal[T, M](dfVal)
  inline def asValOf[T <: DFTypeAny]: DFValOf[T] =
    DFVal[T, ModifierAny](dfVal)
  inline def asValAny: DFValAny =
    DFVal[DFTypeAny, ModifierAny](dfVal)
  inline def asVarOf[T <: DFTypeAny]: DFVarOf[T] =
    DFVal[T, Modifier.VAR](dfVal)
  inline def asVarAny: DFVarAny =
    DFVal[DFTypeAny, Modifier.VAR](dfVal)
  inline def asPortOf[T <: DFTypeAny]: DFPortOf[T] =
    DFVal[T, Modifier.Port](dfVal)

object DFVal:
  inline def unapply(arg: DFValAny): Option[ir.DFVal] = Some(arg.asIR)
  object OrTupleOrStruct:
    def unapply(arg: Any)(using DFC): Option[DFValAny] =
      arg match
        case dfVal: DFValAny     => Some(dfVal)
        case DFTuple.Val(dfVal)  => Some(dfVal)
        case DFStruct.Val(dfVal) => Some(dfVal)
        case _                   => None

  trait Refiner[T <: FieldsOrTuple, A, I]:
    type Out <: DFVal[DFStruct[T], Modifier[A, Any, I]]
  object Refiner:
    transparent inline given [T <: FieldsOrTuple, A, I]: Refiner[T, A, I] = ${
      refineMacro[T, A, I]
    }
    def refineMacro[T <: FieldsOrTuple, A, I](using
        Quotes,
        Type[T],
        Type[A],
        Type[I]
    ): Expr[Refiner[T, A, I]] =
      import quotes.reflect.*
      val dfValTpe = TypeRepr.of[DFVal[DFStruct[T], Modifier[A, Any, I]]]
      val tTpe = TypeRepr.of[T]
      val fields: List[(String, TypeRepr)] = tTpe.asTypeOf[Any] match
        case '[NonEmptyTuple] =>
          tTpe.getTupleArgs.zipWithIndex.map((f, i) =>
            f.asTypeOf[Any] match
              case '[DFValOf[t]] =>
                (s"_${i + 1}", TypeRepr.of[DFVal[t, Modifier[A, Any, I]]])
          )
        case _ =>
          val clsSym = tTpe.classSymbol.get
          clsSym.caseFields.map(m =>
            tTpe.memberType(m).asTypeOf[Any] match
              case '[DFValOf[t]] =>
                (m.name.toString, TypeRepr.of[DFVal[t, Modifier[A, Any, I]]])
          )

      val refined = fields.foldLeft(dfValTpe) { case (r, (n, t)) =>
        Refinement(r, n, t)
      }
      val refinedType = refined.asTypeOf[DFVal[DFStruct[T], Modifier[A, Any, I]]]
      '{
        new Refiner[T, A, I]:
          type Out = refinedType.Underlying
      }
    end refineMacro
  end Refiner

  // Enabling equality with Int, Boolean, and Tuples.
  // just to give a better error message via the compiler plugin.
  // See the method `rejectBadPrimitiveOps` in `MetaContextGenPhase.scala`
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[Int, DFVal[T, M]] =
    CanEqual.derived
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[Boolean, DFVal[T, M]] =
    CanEqual.derived
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[Tuple, DFVal[T, M]] =
    CanEqual.derived

  trait InitCheck[I]
  given [I](using
      initializableOnly: AssertGiven[
        I =:= Modifier.Initializable,
        "Can only initialize a dataflow port or variable that are not already initialized."
      ]
  ): InitCheck[I] with {}

  extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
    def tag[CT <: ir.DFTag: ClassTag](customTag: CT)(using
        dfc: DFC
    ): DFVal[T, M] =
      import dfc.getSet
      dfVal.asIR
        .setTags(_.tag(customTag))
        .setMeta(m => if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta else m)
        .asVal[T, M]
    def setName(name: String)(using dfc: DFC): DFVal[T, M] =
      import dfc.getSet
      dfVal.asIR
        .setMeta(m =>
          if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta.setName(name)
          else m.setName(name)
        )
        .asVal[T, M]
  end extension
  extension [T <: DFTypeAny, A, C, I](dfVal: DFVal[T, Modifier[A, C, I]])
    private[core] def initForced(tokens: List[ir.DFTokenAny])(using
        dfc: DFC
    ): DFVal[T, Modifier[A, C, Modifier.Initialized]] =
      import dfc.getSet
      require(
        dfVal.asIR.isAnonymous,
        s"Cannot initialize a named value ${dfVal.asIR.getFullName}. Initialization is only supported at the declaration of the value."
      )
      dfVal.tag(ir.ExternalInit(tokens)).asIR.asVal[T, Modifier[A, C, Modifier.Initialized]]

    def init(
        tokenValues: DFToken.Value[T]*
    )(using InitCheck[I], DFC): DFVal[T, Modifier[A, C, Modifier.Initialized]] = trydf {
      initForced(tokenValues.view.map(tv => tv(dfVal.dfType).asIR).toList)
    }
  end extension
  extension [T <: NonEmptyTuple, A, C, I](dfVal: DFVal[DFTuple[T], Modifier[A, C, I]])
    def init(
        tokenValues: DFToken.TupleValues[T]
    )(using InitCheck[I], DFC): DFVal[DFTuple[T], Modifier[A, C, Modifier.Initialized]] = trydf {
      dfVal.initForced(tokenValues(dfVal.dfType).map(_.asIR))
    }

  implicit def BooleanHack(from: DFValOf[DFBoolOrBit])(using DFC): Boolean =
    ???
  implicit inline def DFValConversionExact[T <: DFTypeAny, R <: ExactTypes](
      from: R
  )(using dfType: T, es: Exact.Summon[R, from.type])(using
      tc: TC[T, es.Out],
      dfc: DFC
  ): DFValOf[T] = trydf { tc(dfType, es(from)) }

  // opaque values need special conversion that does not try to summon the opaque dftype
  // because it can be abstract in extension methods that are applied generically on an abstract
  // opaque super-type. E.g.:
  // ```
  // abstract class MyAbsOpaque extends Opaque
  // case class MyOpaque extends MyAbsOpaque
  // extension (a : MyAbsOpaque <> VAL) def foo : Unit = {}
  // val a = MyOpaque <> VAR
  // a.foo //here we currently access `foo` through conversion to MyAbsOpaque
  //       //because DFOpaque is not completely covariant due to bug
  //       //https://github.com/lampepfl/dotty/issues/15704
  implicit def DFOpaqueValConversion[T <: DFOpaque.Abstract, R <: DFOpaque.Abstract](
      from: DFValOf[DFOpaque[R]]
  )(using DFC, R <:< T): DFValOf[DFOpaque[T]] = from.asInstanceOf[DFValOf[DFOpaque[T]]]

  implicit def DFValConversion[T <: DFTypeAny, R](
      from: R
  )(using dfType: T)(using
      tc: TC[T, R],
      dfc: DFC
  ): DFValOf[T] = trydf { tc(dfType, from) }

  object Const:
    def apply[T <: DFTypeAny](token: DFToken[T], named: Boolean = false)(using
        DFC
    ): DFValOf[T] =
      val meta = if (named) dfc.getMeta else dfc.getMeta.anonymize
      ir.DFVal
        .Const(token.asIR, dfc.owner.ref, meta, ir.DFTags.empty)
        .addMember
        .asValOf[T]

  object Dcl:
    def apply[T <: DFTypeAny, M <: ModifierAny](dfType: T, modifier: M)(using
        DFC
    ): DFVal[T, M] =
      ir.DFVal
        .Dcl(
          dfType.asIR,
          modifier.asIR,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
        .asVal[T, M]
  end Dcl

  object Func:
    export ir.DFVal.Func.Op
    def apply[T <: DFTypeAny](
        dfType: T,
        op: FuncOp,
        args: List[DFValAny]
    )(using DFC): DFValOf[T] = apply(dfType, op, args.map(_.asIR))
    @targetName("applyFromIR")
    def apply[T <: DFTypeAny](
        dfType: T,
        op: FuncOp,
        args: List[ir.DFVal]
    )(using DFC): DFValOf[T] =
      lazy val func: ir.DFVal = ir.DFVal.Func(
        dfType.asIR,
        op,
        args.map(_.refTW(func)),
        dfc.owner.ref,
        dfc.getMeta,
        ir.DFTags.empty
      )
      func.addMember.asValOf[T]
    end apply
  end Func

  object Alias:
    object AsIs:
      def apply[AT <: DFTypeAny, VT <: DFTypeAny, M <: ModifierAny](
          aliasType: AT,
          relVal: DFVal[VT, M],
          tokenFunc: DFToken[VT] => DFToken[AT],
          forceNewAlias: Boolean = false
      )(using DFC): DFVal[AT, M] =
        relVal.asIR match
          // anonymous constant are replace by a different constant
          // after its token value was converted according to the alias
          case const: ir.DFVal.Const if const.isAnonymous && !forceNewAlias =>
            val updatedToken = tokenFunc(const.token.asTokenOf[VT])
            Const(updatedToken).asIR.asVal[AT, M]
          // named constants or other non-constant values are referenced
          // in a new alias construct
          case _ =>
            forced(aliasType.asIR, relVal.asIR).asVal[AT, M]
      end apply
      def forced(aliasType: ir.DFType, relVal: ir.DFVal)(using DFC): ir.DFVal =
        lazy val alias: ir.DFVal.Alias.AsIs =
          ir.DFVal.Alias.AsIs(
            aliasType,
            relVal.refTW(alias),
            dfc.owner.ref,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember
      def ident[T <: DFTypeAny, M <: ModifierAny](relVal: DFVal[T, M])(using
          DFC
      ): DFVal[T, M] =
        import ir.DFVal.Alias.IdentTag
        apply(relVal.dfType, relVal, x => x, forceNewAlias = true).tag(IdentTag)
      def bind[T <: DFTypeAny, M <: ModifierAny](relVal: DFVal[T, M], bindName: String)(using
          DFC
      ): DFVal[T, M] =
        import ir.DFConditional.DFCaseBlock.Pattern
        ident(relVal)(using dfc.setName(bindName)).tag(Pattern.Bind.Tag)
    end AsIs
    object History:
      export ir.DFVal.Alias.History.Op
      def apply[T <: DFTypeAny](
          relVal: DFValOf[T],
          step: Int,
          op: Op,
          initOption: Option[DFToken[T]]
      )(using DFC): DFValOf[T] =
        lazy val alias: ir.DFVal.Alias.History =
          ir.DFVal.Alias.History(
            relVal.dfType.asIR,
            relVal.asIR.refTW(alias),
            step,
            op,
            initOption.map(_.asIR),
            dfc.owner.ref,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember.asValOf[T]
      end apply
    end History
    object RegDIN:
      def apply[T <: DFTypeAny](relVal: DFValOf[T])(using DFC): DFVarOf[T] =
        lazy val alias: ir.DFVal.Alias.RegDIN =
          ir.DFVal.Alias.RegDIN(
            relVal.dfType.asIR,
            relVal.asIR.refTW(alias),
            dfc.owner.ref,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember.asVarOf[T]
    object ApplyRange:
      def apply[W <: Int, M <: ModifierAny, H <: Int, L <: Int](
          relVal: DFVal[DFBits[W], M],
          relBitHigh: Inlined[H],
          relBitLow: Inlined[L]
      )(using DFC): DFVal[DFBits[H - L + 1], M] =
        relVal.asIR match
          // anonymous constant are replace by a different constant
          // after its token value was converted according to the alias
          case const: ir.DFVal.Const if const.isAnonymous =>
            ???
          // named constants or other non-constant values are referenced
          // in a new alias construct
          case _ =>
            forced(relVal.asIR, relBitHigh, relBitLow).asVal[DFBits[H - L + 1], M]
      end apply
      def forced(
          relVal: ir.DFVal,
          relBitHigh: Int,
          relBitLow: Int
      )(using DFC): ir.DFVal =
        lazy val alias: ir.DFVal.Alias.ApplyRange =
          ir.DFVal.Alias.ApplyRange(
            relVal.refTW(alias),
            relBitHigh,
            relBitLow,
            dfc.owner.ref,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember
      end forced
    end ApplyRange
    object ApplyIdx:
      def apply[T <: DFTypeAny, W <: Int, M <: ModifierAny, IW <: Int](
          dfType: T,
          relVal: DFVal[DFTypeAny, M],
          relIdx: DFUInt[IW] <> VAL
      )(using DFC): DFVal[T, M] =
        lazy val alias: ir.DFVal.Alias.ApplyIdx =
          ir.DFVal.Alias.ApplyIdx(
            dfType.asIR,
            relVal.asIR.refTW(alias),
            relIdx.asIR.refTW(alias),
            dfc.owner.ref,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember.asVal[T, M]
      end apply
    end ApplyIdx
    object SelectField:
      def apply[T <: DFTypeAny, M <: ModifierAny](
          relVal: DFVal[DFTypeAny, M],
          fieldName: String
      )(using dfc: DFC): DFVal[T, M] =
        val relValIR = relVal.asIR
        val ir.DFStruct(_, fieldMap) = relValIR.dfType: @unchecked
        val dfTypeIR = fieldMap(fieldName)
        relValIR match
          // in case the referenced value is anonymous and concatenates fields
          // of values, then we just directly reference the relevant
          // value.
          case ir.DFVal.Func(_, FuncOp.++, args, _, meta, _) if meta.isAnonymous =>
            import dfc.getSet
            val idx = fieldMap.keys.toList.indexWhere(_ == fieldName)
            args(idx).get.asVal[T, M]
          // for all other case create a selector
          case _ =>
            lazy val alias: ir.DFVal.Alias.SelectField =
              ir.DFVal.Alias.SelectField(
                dfTypeIR,
                relValIR.refTW(alias),
                fieldName,
                dfc.owner.ref,
                dfc.getMeta,
                ir.DFTags.empty
              )
            alias.addMember.asVal[T, M]
        end match
      end apply
    end SelectField
  end Alias

  trait TC[T <: DFTypeAny, R] extends TCConv[T, R, DFValAny]:
    type Out = DFValOf[T]
    final def apply(dfType: T, value: R): Out = conv(dfType, value)

  trait TCLP:
    // Accept any bubble value
    given fromBubble[T <: DFTypeAny, V <: Bubble](using
        tokenTC: DFToken.TC[T, V],
        dfc: DFC
    ): TC[T, V] with
      def conv(dfType: T, value: V): DFValOf[T] =
        Const(tokenTC(dfType, value))
    transparent inline given errorDMZ[T <: DFTypeAny, R](using
        t: ShowType[T],
        r: ShowType[R]
    ): TC[T, R] =
      Error.call[
        (
            "Unsupported value of type `",
            r.Out,
            "` for dataflow receiver type `",
            t.Out,
            "`."
        )
      ]
    given sameValType[T <: DFTypeAny, V <: T <> VAL](using DFC): TC[T, V] with
      def conv(dfType: T, value: V): DFValOf[T] =
        given Printer = DefaultPrinter
        given MemberGetSet = dfc.getSet
        require(
          dfType == value.dfType,
          s"Unsupported value of type `${value.dfType.codeString}` for dataflow receiver type `${dfType.codeString}`."
        )
        value
    given sameValAndTokenType[T <: DFTypeAny, V <: T <> TOKEN](using
        DFC
    ): TC[T, V] with
      def conv(dfType: T, value: V): DFValOf[T] =
        given Printer = DefaultPrinter
        given MemberGetSet = dfc.getSet
        require(
          dfType == value.dfType,
          s"Unsupported value of type `${value.dfType.codeString}` for dataflow receiver type `${dfType.codeString}`."
        )
        DFVal.Const(value)
  end TCLP
  object TC extends TCLP

end DFVal
