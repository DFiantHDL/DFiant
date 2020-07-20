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
import DFiant.compiler.{AddTagsOps, Compilation, IRCompilation, PreCompiler}
import DFiant.compiler.backend.BackendStage
import DFiant.csprinter.PrinterOps

import scala.language.experimental.macros
import singleton.ops._
import singleton.ops.impl.HasOut
import DFiant.sim._

package object DFiant {
  type DFBits[W] = DFAny.Of[DFBits.Type[W]]
  type DFBool = DFAny.Of[DFBool.Type]
  type DFBit = DFAny.Of[DFBool.Type]
  type DFUInt[W] = DFAny.Of[DFUInt.Type[W]]
  type DFSInt[W] = DFAny.Of[DFSInt.Type[W]]
  type DFEnum[E <: EnumType] = DFAny.Of[DFEnum.Type[E]]
  type DFString[L] = DFAny.Of[DFString.Type[L]]

  implicit def evPrinterOps[D <: DFDesign, C](c : C)(implicit conv : C => Compilation[D])
  : PrinterOps[D, C] = new PrinterOps[D, C](c)
  implicit class evAddTagOps[D <: DFDesign, H <: shapeless.HList, C](c : C)(
    implicit conv : C => IRCompilation[D, H], externalExtension: ExternalExtension
  ) {
    def !!(tags : TagsOf[D]) = new AddTagsOps[D, H](conv(c)).addTags(tags)
  }

  implicit class BackendExt[D <: DFDesign, H <: shapeless.HList, H2 <: shapeless.HList, T](t : T)(
    implicit conv : T => IRCompilation[D, H]
  ) {
    def compile[B <: BackendStage](
      implicit preCompiler : PreCompiler[D, H, H2], compiler : BackendStage.Compiler[B]
    ) : BackendStage.Compilation[D, B] = compiler(preCompiler(t))
  }
  implicit class SimulatorExt[D <: DFSimDesign, B <: BackendStage](c : BackendStage.CommittedCompilation[D, B]) {
    def simulation[S <: Simulation[D, B]](implicit simulator : Simulator[D, B, S]) : S = simulator(c)
  }

  ////////////////////////////////////////////////////////////////////////////////////
  // A Dataflow Bubble
  ////////////////////////////////////////////////////////////////////////////////////
  sealed trait Bubble
  object Bubble extends Bubble

  type Φ = Bubble
  final val Φ = Bubble
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
  sealed trait DclDir extends DFDir
  //Direction of a Port
  sealed trait PortDir extends DclDir
  case object IN extends PortDir {
    type Func[DF <: DFAny] = DFAny.DefaultRet[DF#TType]
    override def toString: String = "IN "
  }
  type IN = IN.type
  case object OUT extends PortDir {
    type Func[DF <: DFAny] = DFAny.VarOf[DF#TType]
    override def toString: String = "OUT"
  }
  type OUT = OUT.type
  case object VAR extends DclDir {
    type Func[DF <: DFAny] = DFAny.VarOf[DF#TType]
  }
  type VAR = VAR.type
  case object FLIP extends DFDir
  case object ASIS extends DFDir
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
  // BitVector from scodec library https://github.com/scodec/scodec
  // TODO: change after fix for https://github.com/scala/bug/issues/11070
  ////////////////////////////////////////////////////////////////////////////////////
  /*
  Copyright (c) 2013-2014, Michael Pilquist and Paul Chiusano
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
  3. Neither the name of the scodec team nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
   */
  type XInt = singleton.ops.XInt
  type BitVector = scodec.bits.BitVector
  val BitVector = scodec.bits.BitVector
  type XBitVector[W] = DFiant.internals.XBitVector[W]
  final val XBitVector = DFiant.internals.XBitVector
  implicit class XBitVectorExtras[LW](left : XBitVector[LW]) {
    def ++[RW](that : XBitVector[RW])(implicit sum : LW + RW) : XBitVector[sum.OutInt] =
      (left ++ that).asInstanceOf[XBitVector[sum.OutInt]]
  }
  /**
    * Provides the `b` and `h` string interpolator, which returns `BitVector` instances from binary strings.
    */
  final implicit class BinStringSyntax(val sc: StringContext) {
    //    def w[W](args: WidthTag*) : XBitVector[W] = macro Macro.hexStringInterpolator
    /**
      * Converts this binary literal string to a `BitVector`. Whitespace characters are ignored.
      *
      * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
      * of type `BitVector`.
      */
    //    def h[W](args: BitVector*) : XBitVector[W] = macro Macro.hexStringInterpolator
    def h[W](args: BitVector*) : BitVector = macro Macro.hexStringInterpolator

    /**
      * Converts this hexadecimal literal string to a `BitVector`. Whitespace characters are ignored.
      *
      * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
      * of type `BitVector`.
      */
    def b[W](args: BitVector*)(implicit interpolator : Interpolator[BitVector]) : interpolator.Out = interpolator.value

    private def commonInterpolation(args : Seq[Any]) : Seq[Either[DFAny, String]] =
      Seq(sc.parts,args).flatMap(_.zipWithIndex).sortBy(_._2).map(_._1).filter(p => p match {
        case x: String => x.nonEmpty
        case _ => true
      }).map {
        case x : DFAny => Left(x)
        case x => Right(x.toString)
      }
    def msg(args : Any*) : DFSimMember.Assert.Message = DFSimMember.Assert.Message(commonInterpolation(args))
    def vhdl(args : Any*)(implicit ctx : DFAny.Context) : BackendEmitter = BackendEmitter(commonInterpolation(args), compiler.backend.vhdl.Backend)
    def verilog(args : Any*)(implicit ctx : DFAny.Context) : BackendEmitter = BackendEmitter(commonInterpolation(args), compiler.backend.verilog.Backend)
  }
  trait Interpolator[T] extends HasOut {
    type Out <: T
    val value : Out
  }

  object Interpolator {
    type Aux[T, Out0 <: T] = Interpolator[T]{type Out = Out0}
    implicit def ev[W] : Interpolator.Aux[BitVector, XBitVector[W]] = macro Macro.binImplStringInterpolator
  }

  protected object Macro {
    object whitebox { type Context = scala.reflect.macros.whitebox.Context }
    def binImplStringInterpolator(c: whitebox.Context) : c.Tree = {
      import c.universe._
      def calcArgsLength(argsTrees : List[Tree]) : Option[Int] = {
        if (argsTrees.isEmpty) Some(0)
        else {
          val tpes = argsTrees.map(e => e.tpe.dealias)
          val lengths : List[Option[Int]] = tpes.collect {
            case RefinedType(parents, scope) => parents.last.typeArgs.head match {
              case ConstantType(Constant(t : Int)) => Some(t)
              case _ => None
            }
            case _ => None
          }
          def sumOption(a : Option[Int], b : Option[Int]) : Option[Int] = (a, b) match {
            case (Some(aa), Some(bb)) => Some(aa + bb)
            case _ => None
          }
          lengths.reduceLeft(sumOption)
        }
      }

      val Apply(TypeApply(Select(properTree,_), _), argsTrees) = c.enclosingImplicits.last.tree
      val args = argsTrees.map(e => c.Expr[BitVector](e))
      val Apply(_, List(Apply(_, parts))) = properTree
      val partLiterals: List[String] = parts map {
        case Literal(Constant(part: String)) =>
          if (BitVector.fromBin(part).isEmpty)
            c.error(c.enclosingPosition, "binary string literal may only contain characters [0, 1]")
          part
      }
      val length = BitVector.fromBin(partLiterals.head).get.length.toInt

      val headPart = c.Expr[String](Literal(Constant(partLiterals.head)))
      val initialStringBuilder = reify { new StringBuilder().append(headPart.splice) }
      val stringBuilder = (args zip partLiterals.tail).foldLeft(initialStringBuilder) {
        case (sb, (arg, part)) =>
          val partExpr = c.Expr[String](Literal(Constant(part)))
          reify { sb.splice.append(arg.splice.toBin).append(partExpr.splice) }
      }
      val buildTree = reify { BitVector.fromValidBin(stringBuilder.splice.toString) }.tree
      val widthTpe : Type = calcArgsLength(argsTrees) match {
        case Some(t) => c.internal.constantType(Constant(length + t))
        case _ => typeOf[Int]
      }
      q"""
         new DFiant.Interpolator[scodec.bits.BitVector] {
           type Out = DFiant.internals.XBitVector[$widthTpe]
           val value : DFiant.internals.XBitVector[$widthTpe] = $buildTree.asInstanceOf[DFiant.internals.XBitVector[$widthTpe]]
         }
       """
    }

    def hexStringInterpolator(c: whitebox.Context)(args: c.Expr[BitVector]*): c.Tree = {
      import c.universe._

      val Apply(_, List(Apply(_, parts))) = c.prefix.tree
      val partLiterals: List[String] = parts map {
        case Literal(Constant(part: String)) =>
          if (BitVector.fromHex(part).isEmpty)
            c.error(c.enclosingPosition, "binary string literal may only contain characters [0, 1]")
          part
      }
      val length = BitVector.fromHex(partLiterals.head).get.length.toInt

      val headPart = c.Expr[String](Literal(Constant(partLiterals.head)))
      val initialStringBuilder = reify { new StringBuilder().append(headPart.splice) }
      val stringBuilder = (args zip partLiterals.tail).foldLeft(initialStringBuilder) {
        case (sb, (arg, part)) =>
          val partExpr = c.Expr[String](Literal(Constant(part)))
          reify { sb.splice.append(arg.splice.toBin).append(partExpr.splice) }
      }
      val buildTree = reify { BitVector.fromValidHex(stringBuilder.splice.toString) }.tree
      val widthTpe = c.internal.constantType(Constant(length))
      //      q"$buildTree.asInstanceOf[XBitVector[$widthTpe]]"
      q"$buildTree"
    }
  }
  ////////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////////
  // Conditional Constructs
  ////////////////////////////////////////////////////////////////////////////////////
  def ifdf[C](cond : C)(block : => Unit)(
    implicit ctx : DFBlock.Context, condArg : DFBool.Arg[0]
  ) : ConditionalBlock.NoRetVal.IfBlock = ConditionalBlock.NoRetVal.IfBlock(condArg())(block)
  def matchdf[MVType <: DFAny.Type](matchValue : DFAny.Of[MVType], matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(
    implicit ctx : DFBlock.Context
  ): ConditionalBlock.NoRetVal.MatchHeader[MVType] = ConditionalBlock.NoRetVal.MatchHeader[MVType](matchValue, matchConfig)

  implicit class ListExtender[+T](val list : Iterable[T]) {
    def foreachdf[W](sel : DFUInt[W])(block : PartialFunction[T, Unit])(implicit ctx : DFBlock.Context) : Unit = {
      val blockMatchDF = ConditionalBlock.NoRetVal.MatchHeader[DFUInt.Type[W]](sel, MatchConfig.NoOverlappingCases)
      val matcherFirstCase = blockMatchDF.casedf(0)(block(list.head))
      list.drop(1).zipWithIndex.foldLeft(matcherFirstCase)((a, b) => a.casedf(b._2 + 1)(block(b._1))).casedf_{}
    }
    def foreachdf[W](sel : DFBits[W])(block : PartialFunction[T, Unit])(implicit ctx : DFBlock.Context, di : DummyImplicit) : Unit = {
      val blockMatchDF = ConditionalBlock.NoRetVal.MatchHeader[DFBits.Type[W]](sel, MatchConfig.NoOverlappingCases)
      val matcherFirstCase = blockMatchDF.casedf(BigInt(0).toBitVector(sel.width))(block(list.head))
      list.drop(1).zipWithIndex.foldLeft(matcherFirstCase)((a, b) => a.casedf(BigInt(b._2 + 1).toBitVector(sel.width))(block(b._1))).casedf_{}
    }
  }

  implicit class MatchList(list : List[(BitVector, BitVector)]) {
    def matchdf[MW, RW](matchValue : DFBits[MW], resultVar : DFAny.VarOf[DFBits.Type[RW]])(implicit ctx : DFBlock.Context) : Unit = {
      val blockMatchDF = ConditionalBlock.NoRetVal.MatchHeader[DFBits.Type[MW]](matchValue, MatchConfig.NoOverlappingCases)
      if (list.nonEmpty) {
        val matcherFirstCase = blockMatchDF.casedf(list.head._1){resultVar := list.head._2}
        list.drop(1).foldLeft(matcherFirstCase)((a, b) => a.casedf(b._1){resultVar := b._2}).casedf_{}
      }
    }
  }
  ////////////////////////////////////////////////////////////////////////////////////

}
