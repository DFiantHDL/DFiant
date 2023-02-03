package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.targetName
import compiletime.*
import scala.quoted.*
import collection.mutable
import collection.immutable.ListMap
import DFOpaque.Abstract as DFOpaqueA

sealed trait Args
sealed trait NoArgs extends Args
sealed trait Args1[T1] extends Args
sealed trait Args2[T1, T2] extends Args
sealed trait Args3[T1, T2, T3] extends Args

final class DFType[+T <: ir.DFType, +A <: Args](val value: T | DFError) extends AnyVal:
  override def toString: String = value.toString
type DFTypeAny = DFType[ir.DFType, Args]
val NoType = new DFType[ir.NoType.type, NoArgs](ir.NoType)

object DFType:
  type Of[T <: Supported] <: DFTypeAny = T match
    case DFTypeAny => T <:! DFTypeAny
    case Long      => DFSInt[64]
    case Byte      => DFBits[8]
    case Boolean   => DFBool
    case DFOpaqueA => DFOpaque[T]

  type FromDFVal[T] <: DFTypeAny = T match
    case DFValOf[t] => t

  def of[T <: Supported](t: T): Of[T] = DFType(t).asInstanceOf[Of[T]]
  private[core] def apply(t: Any): DFTypeAny =
    t match
      case dfType: DFTypeAny         => dfType
      case tuple: NonEmptyTuple      => DFTuple(tuple)
      case tfe: DFOpaque.Frontend[_] => DFOpaque(tfe)
      case _: Byte.type              => DFBits(8)
      case _: Boolean.type           => DFBool
      case _: Int.type               => DFSInt(32)
      case _: Long.type              => DFSInt(64)
      // TODO: need to add proper upper-bound if fixed in Scalac
      // see: https://contributors.scala-lang.org/t/missing-dedicated-class-for-enum-companions
      case enumCompanion: AnyRef => DFEnum(enumCompanion)
  private[core] def unapply(t: Any): Option[DFTypeAny] = ???

  extension [T <: ir.DFType, A <: Args](dfType: DFType[T, A])
    def asIR: T = dfType.value match
      case dfTypeIR: T @unchecked => dfTypeIR
      case err: DFError           => throw DFError.Derived(err)
  extension (dfType: ir.DFType) def asFE[T <: DFTypeAny]: T = new DFType(dfType).asInstanceOf[T]
  transparent inline implicit def conv[T <: Supported](inline t: T)(implicit
      tc: TC[T]
  ): DFTypeAny = tc(t)
  export DFDecimal.Extensions.*
  export DFBoolOrBit.given
  export DFBits.given
  export DFDecimal.given
  export DFEnum.given
  export DFVector.given

  given [T <: DFTypeAny]: CanEqual[T, T] = CanEqual.derived

  type Supported = DFTypeAny | DFEncoding | DFOpaqueA | Byte | Long | Boolean | AnyRef

  trait TC[T]:
    type Type <: DFTypeAny
    def apply(t: T): Type
  trait TCLP:
    transparent inline given errorDMZ[T](using t: ShowType[T]): TC[T] =
      Error.call[
        (
            "Dataflow type cannot be constructed from the type `",
            t.Out,
            "`."
        )
      ]
  object TC extends TCLP:
    transparent inline given ofDFType[T <: DFTypeAny]: TC[T] = new TC[T]:
      type Type = T
      def apply(t: T): Type = t

    transparent inline given ofBooleanCompanion: TC[Boolean.type] = new TC[Boolean.type]:
      type Type = DFBool
      def apply(t: Boolean.type): Type = DFBool

    transparent inline given ofByteCompanion: TC[Byte.type] = new TC[Byte.type]:
      type Type = DFBits[8]
      def apply(t: Byte.type): Type = DFBits(8)

    transparent inline given ofIntCompanion: TC[Int.type] = new TC[Int.type]:
      type Type = DFSInt[32]
      def apply(t: Int.type): Type = DFSInt(32)

    transparent inline given ofLongCompanion: TC[Long.type] = new TC[Long.type]:
      type Type = DFSInt[64]
      def apply(t: Long.type): Type = DFSInt(64)

    transparent inline given ofOpaque[T <: DFTypeAny, TFE <: DFOpaque.Frontend[T]]: TC[TFE] =
      new TC[TFE]:
        type Type = DFOpaque[TFE]
        def apply(t: TFE): Type = DFOpaque(t)

//    transparent inline given ofDFEnum[E <: DFEncoding]: TC[E] =
//      new TC[E]:
//        type Type = DFEnum[E]
//        def apply(t: E): Type = DFEnum(e)

    transparent inline given ofProductCompanion[T <: AnyRef]: TC[T] = ${ productMacro[T] }
    def productMacro[T <: AnyRef](using Quotes, Type[T]): Expr[TC[T]] =
      import quotes.reflect.*
      val compObjTpe = TypeRepr.of[T]
      val compPrefix = compObjTpe match
        case TermRef(pre, _) => pre
        case _ =>
          report.errorAndAbort("Case class companion must be a term ref")
      val clsSym = compObjTpe.typeSymbol.companionClass
      if !clsSym.paramSymss.forall(_.headOption.forall(_.isTerm)) then
        report.errorAndAbort(
          "Case class with type parameters are not supported"
        )
      val clsTpe = compPrefix.select(clsSym)
      clsTpe.asType match
        case '[t & DFEncoding] =>
          '{
            new TC[T]:
              type Type = DFEnum[t & DFEncoding]
              def apply(t: T): Type = summonInline[DFEnum[t & DFEncoding]]
          }
        case '[t & DFStruct.Fields] =>
          '{
            new TC[T]:
              type Type = DFStruct[t & DFStruct.Fields]
              def apply(t: T): Type =
                summonInline[DFStruct[t & DFStruct.Fields]]
          }
        case '[t & DFOpaque.Abstract] =>
          '{
            new TC[T]:
              type Type = DFOpaque[t & DFOpaque.Abstract]
              def apply(t: T): Type =
                summonInline[DFOpaque[t & DFOpaque.Abstract]]
          }
        case _ =>
          val badTypeStr = clsTpe.show
          val msg =
            if (badTypeStr.endsWith("$package.<none>"))
              s"Type `$badTypeStr` is not a supported dataflow type constructor.\nHint: Are you missing an argument in your dataflow type constructor?"
            else
              s"Type `$badTypeStr` is not a supported product companion.\nHint: Did you forget to extends `Struct` or `Encode`?"
          '{
            compiletime.error(${ Expr(msg) })
            new TC[T]:
              type Type = DFTypeAny
              def apply(t: T): Type = ???
          }
      end match
    end productMacro

    transparent inline given ofTuple[T <: NonEmptyTuple]: TC[T] = ${
      ofTupleMacro[T]
    }
    def ofTupleMacro[T <: NonEmptyTuple](using Quotes, Type[T]): Expr[TC[T]] =
      import quotes.reflect.*
      val tTpe = TypeRepr.of[T]
      val AppliedType(fun, args) = tTpe: @unchecked
      val tcTrees = args.map(t =>
        Implicits.search(TypeRepr.of[TC].appliedTo(t)) match
          case iss: ImplicitSearchSuccess =>
            iss.tree
          case isf: ImplicitSearchFailure =>
            report.errorAndAbort(isf.explanation)
      )
      val tcList = '{
        List(${ Varargs(tcTrees.map(_.asExpr)) }*).asInstanceOf[List[TC[Any]]]
      }
      val tpes = tcTrees
        .map(_.tpe.asTypeOf[Any] match
          case '[TC[t] { type Type = z }] => TypeRepr.of[z]
        )
        .map(t => TypeRepr.of[DFValOf].appliedTo(t))
      def applyExpr(t: Expr[T]): Expr[List[DFTypeAny]] =
        '{
          val tList = $t.toList.asInstanceOf[List[Any]]
          $tcList.lazyZip(tList).map((tc, t) => tc(t)).toList
        }
      val tplTpe = fun.appliedTo(tpes)
      val tplType = tplTpe.asTypeOf[NonEmptyTuple]
      '{
        new TC[T]:
          type Type = DFTuple[tplType.Underlying]
          def apply(t: T): Type =
            DFTuple[tplType.Underlying](${ applyExpr('t) })
      }
    end ofTupleMacro
  end TC
end DFType

extension [T](t: T)(using tc: DFType.TC[T])
  @targetName("tcDFType")
  def dfType: tc.Type = tc(t)

extension [T <: DFTypeAny](token: DFToken[T])
  @targetName("tokenDFType")
  def dfType: T = token.asIR.dfType.asFE[T]

extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
  @targetName("dfValDFType")
  def dfType: T = dfVal.asIR.dfType.asFE[T]
