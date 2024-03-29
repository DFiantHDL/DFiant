/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

import DFiant.internals._
import DFiant.compiler._
import backend.BackendStage
import csprinter.{CSPrinter, CompactCodeString, PrinterOps}

import scala.language.experimental.macros
import singleton.ops._
import singleton.ops.impl.HasOut
import DFiant.sim._

import scala.reflect.ClassTag

package object DFiant {
  final implicit class __MemberSetExtender[T, M <: DFMember](t : T)(implicit tc : DFMember.TC.Aux[T, M]) {
    private val memberContainer = tc(t)
    /**
      * Force a name of a memberContainer.
      * @param value the new name value.
      * @return the memberContainer after the change
      */
    def setName(value : String)(implicit getSet : MemberGetSet) : T =
      memberContainer.setTags(_.setName(value))
    /**
      * Force a suffix attached to the current name
      * @param value the suffix
      * @return the memberContainer after the change
      */
    def setNameSuffix(value : String)(implicit getSet : MemberGetSet) : T =
      setName(s"${memberContainer.name}$value")
    /**
      * Force a prefix attached to the current name
      * @param value the suffix
      * @return the memberContainer after the change
      */
    def setNamePrefix(value : String)(implicit getSet : MemberGetSet) : T =
      setName(s"$value${memberContainer.name}")
    def anonymize(implicit getSet : MemberGetSet) : T = {
      memberContainer.member match {
        //never anonymize declarations
        case _ : DFAny.Dcl => t
        //never anonymize a constant that is already named
        case _ : DFAny.Const => t
        case _ =>
          memberContainer.setTags(_.anonymize)
      }
    }

    def keep(implicit getSet : MemberGetSet) : T =
      memberContainer.setTags(_.setKeep(true))

    /**
      * Tag a memberContainer with the tag `customTag`.
      * Some tags are applicable to any type of memberContainer, others are bounded according the custom tag's bound.
      * @param customTag The tag to be added to the custom tags of the memberContainer.
      *                  If a custom tag of this type already exists, it will be overridden by the new tag.
      * @return the memberContainer after the change
      */
    def tag[CT <: DFMember.CustomTagOf[M] : ClassTag](customTag : CT)(implicit getSet : MemberGetSet) : T =
      memberContainer.setTags(_.tag(customTag))
    def tag(customTags : DFMember.CustomTagMap)(implicit getSet : MemberGetSet) : T =
      memberContainer.setTags(t => t.copy(customTags = t.customTags ++ customTags))
    def removeTagOf[CT <: DFMember.CustomTagOf[M] : ClassTag](implicit getSet : MemberGetSet) : T =
      memberContainer.setTags(_.removeTagOf[CT])
    def getTagOf[CT <: DFMember.CustomTagOf[M] : ClassTag](implicit getSet : MemberGetSet) : Option[CT] =
      memberContainer.tags.getTagOf[CT]
    def isTaggedWith[CT <: DFMember.CustomTagOf[M] : ClassTag](ct : CT)(implicit getSet : MemberGetSet) : Boolean =
      getTagOf[CT].isDefined
    def isTaggedWith[CT <: DFMember.CustomTagOf[M] : ClassTag](implicit getSet : MemberGetSet) : Boolean =
      getTagOf[CT].isDefined
    def setLateConstruction(value : Boolean)(implicit getSet : MemberGetSet) : T =
      memberContainer.setTags(_.setLateConstruction(value))
  }
  final implicit class __MemberGetExtender[T](t : T)(implicit tc : T => DFMember) {
    private val member = tc(t)
    /**
      * @return true if the memberContainer is anonymous
      */
    def isAnonymous : Boolean =
      member.isAnonymous
    /**
      * @return the name of the memberContainer
      */
    def name : String =
      member.name
    def isNameForced : Boolean =
      member.isNameForced
    def hasLateConstruction : Boolean =
      member.hasLateConstruction
    def getFullName(implicit getSet : MemberGetSet) : String =
      member.getFullName
    def getOwner(implicit getSet: MemberGetSet) : DFOwner =
      member.getOwner
    def getOwnerBlock(implicit getSet : MemberGetSet) : DFBlock =
      member.getOwnerBlock
    def getOwnerDesign(implicit getSet : MemberGetSet) : DFDesign.Block =
      member.getOwnerDesign
    def getThisOrOwnerDesign(implicit getSet : MemberGetSet) : DFDesign.Block =
      member.getThisOrOwnerDesign
    def isMemberOfDesign(that : DFDesign.Block)(implicit getSet : MemberGetSet) : Boolean =
      member.isMemberOfDesign(that)
    def isSameOwnerDesignAs[T2](that : T2)(implicit getSet : MemberGetSet, tc : T2 => DFMember) : Boolean =
      member.isSameOwnerDesignAs(tc(that))
    def isOneLevelBelow[T2](that : T2)(implicit getSet : MemberGetSet, tc : T2 => DFMember) : Boolean =
      member.isOneLevelBelow(tc(that))
    def isOutsideOwner(that : DFOwner)(implicit getSet : MemberGetSet) : Boolean =
      member.isOutsideOwner(that)
    def isInsideOwner(that : DFOwner)(implicit getSet : MemberGetSet) : Boolean =
      member.isInsideOwner(that)
  }
  implicit val __memberFromDFMember : DFMember => DFMember = t => t
  implicit val __memberFromDFAny : DFAny => DFAny.Member = _.member
  implicit val __memberFromDFDesign : DFDesign => DFMember = _.owner

  /**
    * Dataflow Bit Vector
    * @tparam W The width of the vector. If the width is known at compile time, then this
    *           argument holds a literal integer type representing it, otherwise, it holds `Int`.
    * @example
    * {{{
    *   val a : DFBits[3]   //width is 3 bits
    *   val b : DFBits[Int] //width isn't known at the Scala compile-time,
    *                       //and checked during DFiant compilation
    * }}}
    */
  type DFBits[W] = DFAny.Of[DFBits.Type[W]]
  /**
    * Dataflow Boolean (equivalent to DFBit)
    */
  type DFBool = DFAny.Of[DFBool.Type]
  /**
    * Dataflow Bit (equivalent to DFBool)
    */
  type DFBit = DFAny.Of[DFBit.Type]
  /**
    * Dataflow Unsigned Integer
    * @tparam W The width of the integer. If the width is known at compile time, then this
    *           argument holds a literal integer type representing it, otherwise, it holds `Int`.
    * @example
    * {{{
    *   val a : DFUInt[3]   //width is 3 bits
    *   val b : DFUInt[Int] //width isn't known at the Scala compile-time,
    *                       //and checked during DFiant compilation
    * }}}
    */
  type DFUInt[W] = DFDecimal[false, W, 0]
  /**
    * Dataflow Signed Integer
    * @tparam W The width of the integer, including the sign bit. If the width is known at compile time, then this
    *           argument holds a literal integer type representing it, otherwise, it holds `Int`.
    * @example
    * {{{
    *   val a : DFSInt[3]   //width is 3 bits
    *   val b : DFSInt[Int] //width isn't known at the Scala compile-time,
    *                       //and checked during DFiant compilation
    * }}}
    */
  type DFSInt[W] = DFDecimal[true, W, 0]
  /**
    * Dataflow Enumeration
    * @tparam E The underlying type of the specific enumeration (an `DFEnum.Entries` object).
    * @example
    * {{{
    *   object Fruit extends DFEnum.Auto {
    *     val Apple, Orange, Peach = Entry()
    *   }
    *   val e : DFEnum[Fruit.type]
    * }}}
    */
  type DFEnum[E <: DFEnum.Entries] = DFAny.Of[DFEnum.Type[E]]
  /**
    * Dataflow General Vector (a vector/array of any other dataflow type)
    * @tparam T The dataflow type element in the vector
    * @tparam N The number of elements in the vector
    */
  type DFVector[T <: DFAny.Type, N] = DFAny.Of[DFVector.Type[T, N]]
  type DFStruct[F <: DFStruct.Fields] = DFAny.Of[DFStruct.Type[F]]
  type DFDecimal[S, W, F] = DFAny.Of[DFDecimal.Type[S, W, F]]
  type DFTuple2[T1 <: DFAny.Type, T2 <: DFAny.Type] = DFAny.Of[DFTuple2.Type[T1, T2]]
  type DFTuple3[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type] = DFAny.Of[DFTuple3.Type[T1, T2, T3]]
  type DFOpaque[F <: DFOpaque.Fields] = DFAny.Of[DFOpaque.Type[F]]

  implicit class __DFOpaqueValue[Mod <: DFAny.Modifier, O <: DFOpaque.Fields](
    val dfOpaque: DFAny.Value[DFStruct.Type[O], Mod],
  )(implicit ctx : DFAny.Context) {
    def actual : DFAny.Value[dfOpaque.dfType.fields.ActualType, Mod] =
      DFStruct.Selector(dfOpaque.dfType.fields.actualType, dfOpaque, "actual")
  }
  implicit class __DFStructFields[F <: DFStruct.Fields, Mod <: DFAny.Modifier](
    val left : DFAny.Value[DFStruct.Type[F], Mod]
  ) {
    def fields : DFStruct.DFFields[F, Mod] = macro DFStruct.DFFields.applyMacro[F, Mod]
  }
  implicit def __DFTuple2Type[
    TO <: DFAny.Type,
    TT1, TT2,
    T1 <: DFAny.Type, T2 <: DFAny.Type
  ](implicit
    tc1 : TT1 => T1, tc2 : TT2 => T2,
    tc : DFTuple2.Fields[T1, T2] => TO
  ) : Tuple2[TT1, TT2] => TO = tuple => tc(new DFTuple2.Fields(
    tc1(tuple._1), tc2(tuple._2)
  ))
  implicit def __DFTuple3Type[
    TO <: DFAny.Type,
    TT1, TT2, TT3,
    T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type
  ](implicit
    tc1 : TT1 => T1, tc2 : TT2 => T2, tc3 : TT3 => T3,
    tc : DFTuple3.Fields[T1, T2, T3] => TO
  ) : Tuple3[TT1, TT2, TT3] => TO = tuple => tc(new DFTuple3.Fields(
    tc1(tuple._1), tc2(tuple._2), tc3(tuple._3)
  ))


  implicit def evPrinterOps[D <: DFDesign, C](c : C)(implicit conv : C => Compilation[D])
  : PrinterOps[D, C] = new PrinterOps[D, C](c)
  implicit class evAddTagOps[D <: DFDesign, C](c : C)(
    implicit conv : C => IRCompilation[D], externalExtension: ExternalExtension
  ) {
    def tag(tags : TagsOf[D]) = new AddTags[D](conv(c)).addTags(tags)
  }

  implicit class BackendExt[D <: DFDesign, T](t : T)(
    implicit conv : T => IRCompilation[D]
  ) {
    /**
      * Dataflow Design Compilation
      *
      * The dataflow DFiant design can be compiled into various backends that are available under [[compiler.backend]].
      * After this step we get a compiled design which can be committed to a folder
      * via [[BackendStage.Compilation.toFolder()]].
      * @param compiler The main backend stage compiler that is implicitly imported into the building program scope.
      *                 E.g., `import compiler.backend.vhdl.v2008`
      * @param preCompiler An optional precompiler that is executed automatically before the backend stage.
      * @param postCompiler An optional postcompiler that is executed automatically after the backend stage.
      * @tparam B The chosen backend compilation stage. Normally this type argument is not applied since only a
      *           single viable backend compiler should be available in the implicit scope.
      * @return a compiled design (not yet committed to files).
      * @example
      * {{{
      *   @df class ID extends DFDesign {
      *     val i = DFUInt(8) <> IN
      *     val o = DFUInt(8) <> OUT
      *     o <> i
      *   }
      *
      *   import compiler.backend.verilog.v2001
      *   val id = new ID
      *   id
      *     .compile //compilation instance
      *     .toFolder("id") //commits the compilation to files
      *                     //in the folder "id"
      * }}}
      */
    def compile[B <: BackendStage](
      implicit preCompiler : PreCompiler[D], compiler : BackendStage.Compiler[B], postCompiler : PostCompiler[D, B]
    ) : BackendStage.Compilation[D, B] = postCompiler(compiler(preCompiler(t)))
  }
  implicit class SimulatorExt[D <: DFSimDesign, B <: BackendStage](c : BackendStage.CommittedCompilation[D, B]) {
    /**
      * For a committed design simulation [[sim.DFSimDesign]], using [[BackendExt.compile]],
      * we can automatically run simulation.
      * @param simulator An implicit simulator imported to scope from [[sim.tools]].
      *                  The simulator capabilities must the chosen backend.
      *                  [[sim.tools.modelsim]] and [[sim.tools.ghdl]] support the VHDL backend.
      *                  [[sim.tools.modelsim]] and [[sim.tools.verilator]] support the Verilog backend.
      * @return a simulation context that can be run
      * @example
      * {{{
      *   @df class ID extends DFDesign {
      *     val i   = DFUInt(8) <> IN
      *     val o   = DFUInt(8) <> OUT
      *     o <> i
      *   }
      *   @df class IDTest extends sim.DFDimDesign {
      *     val id  = new ID
      *     val cnt = DFUInt(8) init 0
      *     id.i <> cnt
      *     sim.report(msg"\$id.o") //will output the count value
      *     cnt := cnt + 1
      *   }
      *
      *   import compiler.backend.verilog.v2001
      *   import sim.tools.verilator
      *   val idTest = new IDTest
      *   idTest
      *     .compile //compilation instance
      *     .toFolder("id") //commits files to folder
      *     .simulation //creates a simulation context
      *     .run() //runs verilator
      * }}}
      */
    def simulation[S <: Simulation[D, B]](implicit simulator : Simulator[D, B, S]) : S = simulator(c)
  }

  ////////////////////////////////////////////////////////////////////////////////////
  // A Dataflow Bubble
  ////////////////////////////////////////////////////////////////////////////////////
  sealed trait Bubble
  /**
    * A named bubble construct. See [[?]].
    */
  object Bubble extends Bubble {
    sealed trait Behaviour extends Product with Serializable
    implicit case object Stall extends Behaviour
    case object DontCare extends Behaviour
  }

  type ? = Bubble
  /**
    * The bubble construct. Bubbles are used to declare either stalls or "don't cares",
    * depending on the context in which they are applied
    */
  final val ? = Bubble
  ////////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////////
  // Dataflow Port Annotations
  ////////////////////////////////////////////////////////////////////////////////////
  sealed trait DFDir extends Product with Serializable {
    type Func[DF <: DFAny]
  }
  type <>[DF <: DFAny, Dir <: DFDir] = Dir#Func[DF]
//  protected[DFiant] type <~>[DF <: DFAny, Dir <: DFDir] = DFAny.Port[DF#TType, Dir]

  //Declaration directionality (Var/PortDir)
  sealed trait DclDir extends DFDir {
    import DFAny.Modifier
    private[DFiant] def getModifier(implicit ctx : DFAny.Context) : DFAny.Modifier = {
      ctx.dir match {
        case d : PortDir => Modifier.Port(d)
        case DFiant.VAR => Modifier.NewVar
        case DFiant.FLIP => this match {
          case IN => Modifier.Port(OUT)
          case OUT => Modifier.Port(IN)
          case VAR => Modifier.NewVar
        }
        case DFiant.ASIS => this match {
          case IN => Modifier.Port(IN)
          case OUT => Modifier.Port(OUT)
          case VAR => Modifier.NewVar
        }
      }
    }
  }
  //Direction of a Port
  sealed trait PortDir extends DclDir

  /**
    * Input Port/Interface Direction
    */
  case object IN extends PortDir {
    type Func[DF <: DFAny] = DFAny.Of[DF#TType]
    override def toString: String = "IN "
  }
  type IN = IN.type

  /**
    * Output Port/Interface Direction
    */
  case object OUT extends PortDir {
    type Func[DF <: DFAny] = DFAny.VarOf[DF#TType]
    override def toString: String = "OUT"
  }
  type OUT = OUT.type

  /**
    * Variable Declaration
    */
  case object VAR extends DclDir {
    type Func[DF <: DFAny] = DFAny.VarOf[DF#TType]
  }
  type VAR = VAR.type

  /**
    * Flipping the Direction of a Port/Interface
    */
  case object FLIP extends DFDir

  /**
    * Leaving the Direction of a Port/Interface as-is
    */
  case object ASIS extends DFDir

  protected[DFiant] trait DEFAULT_DIR {
    private[DFiant] var currentDefault : DclDir = VAR
    def get : DclDir = currentDefault
    def <> (dir : DclDir) : Unit = currentDefault = dir
  }
  ////////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////////
  // Intervals
  ////////////////////////////////////////////////////////////////////////////////////
  type Interval[T] = continuum.Interval[T]
  final val Interval = continuum.Interval
  type IntervalSet[T] = continuum.IntervalSet[T]
  final val IntervalSet = continuum.IntervalSet
  ////////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////////
  // Various String Interpolators
  ////////////////////////////////////////////////////////////////////////////////////
  final implicit class BinStringSyntax(val sc: StringContext) {
    /**
      * Binary Bits Vector Token String Interpolator
      *
      * Interpolator Syntax: {{{b"width'bin"}}}
      *  - `bin` is a char sequence of '0', '1', and '?' (to indicate a bit bubble).
      *  - `bin` also allows separators of underscore '_' that are ignored.
      *  - `width` (with the following tag char `'`) is optional. If it's not specified, the width is
      *  determined by the length of the char sequence. Otherwise, the width is set as required.
      *  If the required width is longer than the char sequence length, then zeros are added as the MSBits.
      *  If the required width is shorter then the char sequence, it is accepted only if the MSBits it is
      *  truncating are zeros. Otherwise, a compilation error is generated.
      * @example
      * {{{
      *   b"1"        //value = 1
      *   b"1000"     //value = 1000
      *   b"8'1000"   //value = 00001000
      *   b"3'0100"   //value = 100
      *   b"3'1100"   //error
      *   b"1?11"     //value = 1?11 (? is a bubble bit)
      *   b"11_00"    //value = 1100
      * }}}
      * @note The string interpolator currently does not accept external arguments with `\${arg}`
      * @return Bits vector token.
      */
    def b[W](args: Any*)(
      implicit interpolator : Interpolator[DFBits.Token, "b"]
    ) : interpolator.Out = interpolator.value
    /**
      * Hexadecimal Bits Vector Token String Interpolator
      *
      * Interpolator Syntax: {{{b"width'hex"}}}
      *  - `hex` is a char sequence of '0'-'9','A'-'F','a'-'f','?' (to indicate a 4-bit bubble).
      *  Each character is equivalent to a 4-bits nibble.
      *  - `hex` also allows separators of underscore '_' that are ignored.
      *  - `hex` also supports a binary mode within `{bin}`, where bin is equivalent to the char sequence of
      *  the binary string interpolator (see [[b]]). So between 4-bit hex nibbles, it is possible to insert
      *  a binary bit sequence of any length that is not necessarily dividable by 4.
      *  - `width` (with the following tag char `'`) is optional. If it's not specified, the width is
      *  determined by the length of the char sequence. Otherwise, the width is set as required.
      *  If the required width is longer than the char sequence length, then zeros are added as the MSBits.
      *  If the required width is shorter then the char sequence, it is accepted only if the MSBits it is
      *  truncating are zeros. Otherwise, a compilation error is generated.
      * @example
      * {{{
      *   h"1"        //value = 0001
      *   h"27"       //value = 00100111
      *   h"6'27"     //value = 100111
      *   h"5'27"     //error
      *   h"2?"       //value = 0010????
      *   h"F{00}F"   //value = 1111001111
      *   h"3_3"      //value = 00110011
      * }}}
      * @note The string interpolator currently does not accept external arguments with `\${arg}`
      * @return Bits vector token.
      */
    def h[W](args: Any*)(
      implicit interpolator : Interpolator[DFBits.Token, "h"]
    ) : interpolator.Out = interpolator.value

    def d[W](args: Any*)(
      implicit interpolator : Interpolator[DFDecimal.Token, "d"]
    ) : interpolator.Out = interpolator.value

    private def commonInterpolation(args : Seq[Any]) : Seq[Either[DFAny.Member, String]] =
      Seq(sc.parts,args).flatMap(_.zipWithIndex).sortBy(_._2).map(_._1).filter(p => p match {
        case x: String => x.nonEmpty
        case _ => true
      }).map {
        case x : DFAny => Left(x.member)
        case x => Right(x.toString)
      }

    /**
      * Simulation Message String Interpolator
      *
      * `sim.assert` and `sim.report` accept a `Message` argument. This interpolator constructs such a message.
      * If the interpolator arguments are dataflow variables, the backend compiler connects those with the
      * actual values readable by the simulation. Otherwise, the compile converts the variable with a
      * Scala `.toString` and places that value in the outgoing message.
      * @example
      * {{{
      *   val a = DFUInt(8)
      *   a := 55
      *   val x = "nice!"
      *   sim.report(msg"a = \$a \$x") //In simulation prints out: a = 55 nice!
      * }}}
      * @return `Message` for simulation printout
      */
    def msg(args : Any*) : DFSimMember.Assert.Message = DFSimMember.Assert.Message(commonInterpolation(args))
    def cs(args : Any*)(implicit ctx : DFAny.Context) : CompactCodeString = {
      val seq = Seq(sc.parts,args).flatMap(_.zipWithIndex).sortBy(_._2).map(_._1).filter(p => p match {
        case x: String => x.nonEmpty
        case _ => true
      }).map {
        case x : DFAny => CompactCodeString.MemberPart(x.member)
        case x : DFAny.Member => CompactCodeString.MemberPart(x)
        case CSFunc(func) => CompactCodeString.CSPrintPart(func)
        case x => CompactCodeString.StringPart(x.toString)
      }
      CompactCodeString(seq)
    }
    /**
      * Experimental. Do not use.
      */
    def vhdl(args : Any*)(
      implicit ctx : DFAny.Context
    ) : BackendEmitter = BackendEmitter(commonInterpolation(args), compiler.backend.vhdl.Backend)
    /**
      * Experimental. Do not use.
      */
    def verilog(args : Any*)(
      implicit ctx : DFAny.Context
    ) : BackendEmitter = BackendEmitter(commonInterpolation(args), compiler.backend.verilog.Backend)
  }
  trait Interpolator[T, K] extends HasOut {
    type Out <: T
    val value : Out
  }

  object Interpolator {
    type Aux[T, K, Out0 <: T] = Interpolator[T, K]{type Out = Out0}
    implicit def evb[W] : Interpolator.Aux[DFBits.Token, "b", DFBits.TokenW[W]] =
      macro DFBits.Token.binImplStringInterpolator
    implicit def evh[W] : Interpolator.Aux[DFBits.Token, "h", DFBits.TokenW[W]] =
      macro DFBits.Token.hexImplStringInterpolator
    implicit def evd[S, W, F] : Interpolator.Aux[DFDecimal.Token, "d", DFDecimal.TokenW[S, W, F]] =
      macro DFDecimal.Token.decImplStringInterpolator
  }
  final case class CSFunc(func : CSPrinter.Config => String)
  ////////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////////
  // Conditional Constructs
  ////////////////////////////////////////////////////////////////////////////////////
  /**
    * Dataflow If Construct
    *
    * @param cond A dataflow boolean expression
    * @param block The block that should be active when the condition is met
    * @return an [[DFConditional.NoRetVal.IfElseBlock]] instance that provides access to the additional
    *         [[DFConditional.NoRetVal.IfElseBlock.IfElseBlockOps.elseifdf]] and
    *         [[DFConditional.NoRetVal.IfElseBlock.IfElseBlockOps.elsedf]] conditional branches.
    * @example
    * {{{
    *   val b = DFBool() <> IN
    *   val c = DFUInt(8) <> OUT
    *   ifdf(b){
    *     c := 5
    *   }.elsedf {
    *     c := 7
    *   }
    * }}}
    */
  def ifdf[C](cond : Exact[C])(block : => Unit)(
    implicit ctx : DFBlock.Context, condArg : DFBool.Arg[C]
  ) : DFConditional.NoRetVal.IfElseBlock[true] =
    new DFConditional.NoRetVal.IfElseBlock[true](Some(condArg(cond)), None)(block)

  /**
    * Pattern Matching (Case) Construct
    *
    * @param matchValue A dataflow value to pattern match on. Each dataflow variable supports different kind of patterns
    * @param matchConfig To configure whether or not the match allows overlapping cases
    * @return a [[DFConditional.NoRetVal.MatchHeader]] instance that provides access to the additional
    *         [[DFConditional.NoRetVal.HasCaseDF.CaseBlockOps.casedf]] and
    *         [[DFConditional.NoRetVal.HasCaseDF.CaseBlockOps.casedf(?)]] conditional branches.
    * @example
    * {{{
    *   val b = DFUInt(8) <> IN
    *   val c = DFUInt(8) <> OUT
    *   matchdf(c)
    *     .casedf(1 to 10) {c := 1}
    *     .casedf(20 to 22, 27 to 29) {c := 2}
    *     .casedf(?) {c := 3}
    * }}}
    */
  def matchdf[MVType <: DFAny.Type](matchValue : DFAny.Of[MVType], matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(
    implicit ctx : DFBlock.Context
  ): DFConditional.NoRetVal.MatchHeader[MVType] =
    new DFConditional.NoRetVal.MatchHeader[MVType](matchValue, matchConfig)

  implicit class ListExtender[+T](val list : Iterable[T]) {
    def foreachdf[W](sel : DFUInt[W])(block : PartialFunction[T, Unit])(implicit ctx : DFBlock.Context) : Unit = {
      val blockMatchDF = new DFConditional.NoRetVal.MatchHeader[DFUInt.Type[W]](sel, MatchConfig.NoOverlappingCases)
      val matcherFirstCase = blockMatchDF.casedf(0)(block(list.head))
      val cases = list.drop(1).zipWithIndex.foldLeft(matcherFirstCase)((a, b) => a.casedf(b._2 + 1)(block(b._1)))
      if (sel.width.getValue != (list.size-1).bitsWidth(false)) cases.casedf(?){}
    }
    def foreachdf[W](sel : DFBits[W])(block : PartialFunction[T, Unit])(implicit ctx : DFBlock.Context, di : DummyImplicit) : Unit = {
      val blockMatchDF = new DFConditional.NoRetVal.MatchHeader[DFBits.Type[W]](sel, MatchConfig.NoOverlappingCases)
      val matcherFirstCase = blockMatchDF.casedf(DFBits.Token(sel.width.getValue, BigInt(0)))(block(list.head))
      val cases = list.drop(1).zipWithIndex.foldLeft(matcherFirstCase)((a, b) => a.casedf(DFBits.Token(sel.width.getValue, BigInt(b._2 + 1)))(block(b._1)))
      if (sel.width.getValue != (list.size-1).bitsWidth(false)) cases.casedf(?){}
    }
  }

  implicit class MatchList(list : List[(DFBits.Token, DFBits.Token)]) {
    import DFDesign.Frontend._
    def matchdf[MW, RW](matchValue : DFBits[MW], resultVar : DFAny.VarOf[DFBits.Type[RW]])(implicit ctx : DFBlock.Context) : Unit = {
      val blockMatchDF = new DFConditional.NoRetVal.MatchHeader[DFBits.Type[MW]](matchValue, MatchConfig.NoOverlappingCases)
      if (list.nonEmpty) {
        val matcherFirstCase = blockMatchDF.casedf(list.head._1){resultVar := list.head._2}
        val cases = list.drop(1).foldLeft(matcherFirstCase)((a, b) => a.casedf(b._1){resultVar := b._2})
        if (matchValue.width.getValue != (list.size-1).bitsWidth(false)) cases.casedf(?){}
      }
    }
  }
  ////////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////////
  // Exception handling for DFiant code errors
  ////////////////////////////////////////////////////////////////////////////////////
  def errordf(msg : String)(implicit ctx : DFMember.Context) : Nothing = {
    import scala.io.AnsiColor.{RED, RESET}
    val pos = ctx.meta.position
    val fullName =
      if (ctx.meta.isAnonymous) ctx.owner.getFullName
      else s"${ctx.owner.getFullName}.${ctx.meta.name}"
    println(s"${RED}DFiant compilation failed at:\n$pos\nin:\n$fullName\nwith:\n$msg$RESET")
    sys.exit(1)
  }
  def trydf[T](block : => T)(implicit ctx : DFMember.Context) : T = try {
    block
  } catch {
    case e : IllegalArgumentException =>
      errordf(e.getMessage.replaceFirst("requirement failed: ", ""))
  }
  ////////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////////
  // FSM Operations
  ////////////////////////////////////////////////////////////////////////////////////
  protected sealed class FSMOps(srcBlock : => Unit) {

    /**
      * Destination Step Edge Connection
      *
      * Constructs both the LHS and RHS into FSMs and adds a connection from the LHS step to the RHS step.
      * @param dstBlock A block of statements to become the body the first step in the RHS FSM.
      * @return an FSM with the additional exit condition edge
      */
    def ==> [T2 <: FSM.Capable](dstBlock : => T2)(
      implicit ctx : DFBlock.Context, ta : FSM.HasFSMAscription, di : DummyImplicit
    ) : FSM = FSM(srcBlock) ==> dstBlock

    /**
      * Exit-Condition Edge Connection
      *
      * Constructs the LHS into an FSM and adds a conditional exit edge according the given condition.
      * Must be followed by a connecting edge [[FSM.==>]] to set the destination step.
      * @param cond A dataflow boolean expression to condition the exit on.
      * @return an FSM with the additional exit condition edge
      */
    @targetName("nextIf")
    def =?> [C](cond : Exact[C])(
      implicit ctx : DFBlock.Context, ta : FSM.HasFSMAscription, arg : DFBool.Arg[C]
    ) : FSM = FSM {
      srcBlock
      ifdf(arg(cond)) {
        nextStep.goto()
      }
    }
    /**
      * Destination Step Edge Connection
      *
      * Constructs the LHS into an FSM and adds a connection from the LHS step to the RHS FSM beginner step.
      * @param dstFSM The destination FSM
      * @return an FSM with the additional exit condition edge
      */
    def ==> (dstFSM : => FSM)(
      implicit ctx : DFBlock.Context, ta : FSM.HasFSMAscription
    ) : FSM = FSM(srcBlock) ==> dstFSM
  }
  final implicit class FSMOpsUnit(srcBlock : => Unit) extends FSMOps(srcBlock)
  final implicit class FSMOpsCapable[T <: FSM.Capable](srcBlock : => T) extends FSMOps(srcBlock)

  /**
    * @return a reference to the next step in the FSM, assuming one will be connected.
    *         If there is no connection for the next step, an error will be thrown.
    */
  def nextStep(implicit ctx : DFAny.Context) : FSM.Step = ctx.db.getNextFSMStep match {
    case Some(value) => value
    case None => throw new IllegalArgumentException("\nMissing next step `==>` connection")
  }
  ////////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////////
  // Collection Operations
  ////////////////////////////////////////////////////////////////////////////////////
  implicit class IterableOps[T <: DFAny.Type](iter : Iterable[DFAny.Of[T]]) {
    import DFDesign.Frontend._

    /**
      * @return a dataflow boolean true indication if all iteration elements are equal in value
      */
    def allAreEqual(implicit ctx : DFBlock.Context) : DFBool = {
      val split = iter.map(_.bits).splitAt(iter.size/2)
      (split._1 lazyZip split._2).map(_ === _).reduce(_ && _)
    }

    /**
      * @param sel the requested selection within the iteration
      * @return a selected element within the iteration
      */
    def selectdf(sel : DFUInt[Int])(implicit ctx : DFBlock.Context) : DFAny.Of[T] = {
      val ret = iter.head.asNewVar
      iter.foreachdf(sel) {q => ret := q}
      ret
    }
  }
  ////////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////////
  // Collection Operations
  ////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] final case class OutsideOwner(op : DFDesign.Control.Op.Entry) extends DFMember.CustomTagOf[DFMember]
  final val OutsideOwnerEnable = OutsideOwner(DFDesign.Control.Op.Enable)
  final val OutsideOwnerStall = OutsideOwner(DFDesign.Control.Op.Stall)
  final val OutsideOwnerClear = OutsideOwner(DFDesign.Control.Op.Clear)
  ////////////////////////////////////////////////////////////////////////////////////

}
