package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import DFAny.Func2
import DFiant.csprinter.{CSPrinter, CodeStringOf}

import scala.annotation.nowarn

object b0s extends DFBits.SameBitsVector(false)
object b1s extends DFBits.SameBitsVector(true)

object DFBits extends DFAny.Companion {
  final case class Type[W](width : TwoFace.Int[W]) extends DFAny.Type {
    type Width = W
    type TToken = Token
    type TPattern = DFBits.Pattern
    type TPatternAble[+R] = DFBits.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFBits.Pattern.Builder[LType]
    type OpAble[R] = DFBits.Op.Able[R]
    type `Op==Builder`[L, R] = DFBits.`Op==`.Builder[L, R]
    type `Op!=Builder`[L, R] = DFBits.`Op!=`.Builder[L, R]
    type `Op<>Builder`[LType <: DFAny.Type, R] = DFBits.`Op<>`.Builder[LType, R]
    type `Op:=Builder`[LType <: DFAny.Type, R] = DFBits.`Op:=`.Builder[LType, R]
    type InitAble[L <: DFAny] = DFBits.Init.Able[L]
    type InitBuilder[L <: DFAny] = DFBits.Init.Builder[L, TToken]
    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token = fromToken
    def assignCheck(from : DFAny)(implicit ctx : DFAny.Context) : Unit = from match {
      case r @ DFBits(w) =>
        val op = implicitly[`Op:=`.Builder[Type[W], DFBits[Int]]]
        op(this, r.asInstanceOf[DFBits[Int]])
    }
    def valueCodeString(value : BitVector)(implicit printer : CSPrinter) : String = ???
    def valueToBitVector(value : BitVector) : BitVector = value
    override def toString: String = s"DFBits[$width]"
    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      s"$TP DFBits($LIT$width)"
    }
    override def equals(obj: Any): Boolean = obj match {
      case Type(width) => this.width.getValue == width.getValue
      case _ => false
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[W](checkedWidth : BitsWidth.Checked[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(checkedWidth.unsafeCheck()))
  def apply[W](
    implicit ctx : DFAny.Context, checkedWidth : BitsWidth.Checked[W], di: DummyImplicit
  ) = DFAny.NewVar(Type(checkedWidth))

  def unapply(arg: DFAny): Option[Int] = arg.dfType match {
    case Type(width) => Some(width.getValue)
    case _ => None
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type TokenW[W] = Token with WidthTag[W]
  final case class Token(valueBits : BitVector, bubbleMask : BitVector) extends DFAny.Token {left =>
    assert(valueBits.length == bubbleMask.length)
    val width : Int = valueBits.length.toInt
    def & (right : Token)(implicit bb : Bubble.Behaviour) : Token = {
      assert(right.width == width)
      bb match {
        case Bubble.Stall =>
          Token(left.valueBits & right.valueBits, left.bubbleMask | right.bubbleMask)
        case Bubble.DontCare =>
          val valueBits = (left.valueBits | left.bubbleMask) & (right.valueBits | right.bubbleMask)
          val bubbleMask = (left.bubbleMask & right.bubbleMask) | (left.bubbleMask & right.valueBits) |
            (right.bubbleMask & left.valueBits)
          Token(valueBits, bubbleMask)
      }
    }
    def | (right : Token)(implicit bb : Bubble.Behaviour) : Token = {
      assert(right.width == width)
      bb match {
        case Bubble.Stall =>
          Token(left.valueBits & right.valueBits, left.bubbleMask | right.bubbleMask)
        case Bubble.DontCare =>
          val valueBits = (left.valueBits & left.bubbleMask.unary_~()) | (right.valueBits & right.bubbleMask.unary_~())
          val bubbleMask = (left.bubbleMask & right.bubbleMask) | (left.bubbleMask & right.valueBits.unary_~()) |
            (right.bubbleMask & left.valueBits.unary_~())
          Token(valueBits, bubbleMask)
      }
    }
    //dontcare in xor will always produce dontcare, like stall bubbles
    def ^ (right : Token) : Token = {
      assert(right.width == width)
      val valueBits = left.valueBits ^ right.valueBits
      val bubbleMask = left.bubbleMask | right.bubbleMask
      Token(valueBits, bubbleMask)
    }
    def ++ (right : Token) : Token = {
      Token(left.valueBits ++ right.valueBits, left.bubbleMask ++ right.bubbleMask)
    }
    def << (shift : DFUInt.Token) : Token = shift.value match {
      case Some(value) => Token(left.valueBits << value.toInt, left.bubbleMask << value.toInt)
      case None => Token.bubble(width)
    }
    def >> (shift : DFUInt.Token) : Token = shift.value match {
      case Some(value) => Token(left.valueBits >>> value.toInt, left.bubbleMask >>> value.toInt)
      case None => Token.bubble(width)
    }
    def unary_~ : Token = Token(left.valueBits.unary_~(), left.bubbleMask)
    def reverse : Token = Token(left.valueBits.reverseBitOrder, left.bubbleMask.reverseBitOrder)
    def resize(toWidth : Int) : Token = {
      if (toWidth < width) bitsWL(toWidth, 0)
      else if (toWidth > width) (Token.zero(toWidth - width) ++ this)
      else this
    }
    def == (right : Token)(implicit bb : Bubble.Behaviour) : DFBool.Token = {
      assert(right.width == width)
      bb match {
        case Bubble.Stall => 
          if (left.isBubble || right.isBubble) DFBool.Token.bubble(logical = true)
          else DFBool.Token(logical = true, left.valueBits == right.valueBits)
        case Bubble.DontCare =>
          val valueBits = (left.bubbleMask | right.bubbleMask | (left.valueBits ^ right.valueBits).unary_~()) == BitVector.high(width)
          DFBool.Token(logical = true, valueBits)
      }
    }
    def != (that : Token)(implicit bb : Bubble.Behaviour) : DFBool.Token = !(this == that)

    def toUInt : DFUInt.Token = {
      if (isBubble) DFUInt.Token.bubble(width)
      else DFUInt.Token(width, BigInt(this.valueBits.padToMulsOf(8).toByteArray).asUnsigned(width))
    }
    def toSInt : DFSInt.Token = {
      if (isBubble) DFSInt.Token.bubble(width)
      else DFSInt.Token(width, BigInt(this.valueBits.padToMulsOf(8).toByteArray))
    }
    def getUIntValue : BigInt = valueBits.toBigInt.asUnsigned(width)
    def getSIntValue : BigInt = valueBits.toBigInt
    def toBinString : String = {
      val valueString = valueBits.toBin
      val bubbleString = bubbleMask.toBin
      valueString.zip(bubbleString).map {
        case (_, '1') => '?'
        case (zeroOrOne, _) => zeroOrOne
      }.mkString
    }
    def toHexString : String = {
//      if (width % 4 == 0)
      val valueString = valueBits.toHex
      val bubbleString = bubbleMask.toHex
      valueString.zip(bubbleString).map {
        case (_, 'F') => '?'
        case (h, '0') => h
      }.mkString
    }
    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      import io.AnsiColor.BOLD
      if (width % 4 == 0) s"""$BOLD h$STR"${valueBits.toHex}""""
      else s"""$BOLD b$STR"${valueBits.toBin}""""
    }
  }
  object Token {
    implicit val bubbleOfToken : DFAny.Token.BubbleOfToken[Token] = t => bubble(t.width)
    implicit def bubbleOfDFType[W] : DFAny.Token.BubbleOfDFType[Type[W]] = t => bubble(t.width.getValue)
    def zero(width : Int) : Token = Token(BitVector.low(width))
    def apply(value : BitVector) : Token = Token(value, BitVector.low(value.length))
    def apply(width : Int, value : BigInt) : Token = Token(value.toBitVector(width))
    def bubble(width : Int) : Token = Token(BitVector.low(width), BitVector.high(width))
    def fromBinString(bin : String) : Option[Token] = {
      val noUnderscore = bin.replaceAll("_", "")
      val valuePart = noUnderscore.replace('?', '0')
      val bubblePart = noUnderscore.replaceAll("[1|0]","0").replace('?','1')
      val valueBitsOption = BitVector.fromBin(valuePart)
      val bubbleBitsOption = BitVector.fromBin(bubblePart)
      (valueBitsOption, bubbleBitsOption) match {
        case (Some(v), Some(b)) => Some(Token(v, b))
        case _ => None
      }
    }

    def fromHexString(hex : String) : Option[Token] = {
      val isHex = "[0-9a-fA-F]".r
      val (valueBits, bubbleMask, binMode) = hex.foldLeft((BitVector.empty, BitVector.empty, false)) {
        case (t, '_') => t //ignoring underscore
        case ((v, b, false), c) => c match { //hex mode
          case '{' => (v, b, true)
          case '?' => (v ++ BitVector.low(4), b ++ BitVector.high(4), false)
          case isHex() => (v ++ BitVector.fromHex(c.toString).get, b ++ BitVector.low(4), false)
          case _ => return None
        }
        case ((v, b, true), c) => c match { //bin mode
          case '}' => (v, b, false)
          case '?' => (v :+ false, b :+ true, true)
          case '0' => (v :+ false, b :+ false, true)
          case '1' => (v :+ true, b :+ false, true)
          case _ => return None
        }
      }
      if (binMode) None
      else Some(Token(valueBits, bubbleMask))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private val patternCodeString : CodeStringOf[Token] = new CodeStringOf[Token] {
    def apply(t : Token)(implicit printer : CSPrinter) : String = t.codeString
  }
  class Pattern(set : Set[Token]) extends DFAny.Pattern.OfSet[Token, Pattern](set)(patternCodeString)
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R] {
      val token : Token
    }
    object Able {
      implicit class DFBitsPattern[R <: Token](val right : R) extends Able[R] {
        val token : Token = right
      }
    }
    trait Builder[LType <: DFAny.Type] extends DFAny.Pattern.Builder[LType, Able]
    object Builder {
      implicit def ev[LW] : Builder[Type[LW]] = new Builder[Type[LW]] {
        def apply[R](left: Type[LW], right: Seq[Able[R]]): Pattern = {
          val patternSet = right.map(e => e.token).foldLeft(Set.empty[Token])((set, token) => {
            if (set.contains(token)) throw new IllegalArgumentException(s"\nThe bitvector $token already intersects with $set")
            if (token.width > left.width) throw new IllegalArgumentException(s"\nThe bitvector $token is wider than ${left.width}")
            set + token
          })

          new Pattern(patternSet)
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends InitCO {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      trait VerifyWidth[T, W]
      object VerifyWidth {
        implicit def ev[W] : VerifyWidth[Token, W] = new VerifyWidth[Token, W]{}
        implicit def evX[W, XW](implicit req : Require[W == XW]) : VerifyWidth[TokenW[XW], W] = new VerifyWidth[TokenW[XW], W]{}
      }
      implicit class DFBitsBubble[LW](val right : Bubble) extends Able[DFBits[LW]]
      implicit class DFBitsSameBitsVector[LW](val right : SameBitsVector) extends Able[DFBits[LW]]
      implicit class DFBitsToken[LW, R](val right : Token)(implicit arg : GetArg0.Aux[R], req : VerifyWidth[R, LW]) extends Able[DFBits[LW]]
      implicit class DFBitsTokenSeq[LW](val right : Seq[Token]) extends Able[DFBits[LW]]

      private def checkWidth(leftWidth : Int, rightWidth : Int) : Unit =
        if (leftWidth != rightWidth)
          throw new IllegalArgumentException(s"Init value width $rightWidth doesn't match the vector width ${leftWidth}")
      def toTokenSeq[LW](width : Int, right : Seq[Able[DFBits[LW]]]) : Seq[Token] =
        right.toSeqAny.collect {
          case t : Bubble => Token.bubble(width)
          case t : Token => checkWidth(width, t.width); t
          case t : SameBitsVector => Token(XBitVector.fill(width)(t.value))
        }
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[LW] : Builder[DFBits[LW], Token] = (left, right) => Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Builder
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type Const[W] = DFAny.Const.Of[Type[W]]
  object Const {
    trait Builder[N] {
      type W
      def apply(value : N) : Const[W]
    }
    object Builder {
      type Aux[N, W0] = Builder[N]{type W = W0}
      implicit def fromToken(implicit ctx : DFAny.Context)
      : Aux[Token, Int] = new Builder[Token] {
        type W = Int
        def apply(value : Token) : Const[W] = {
          DFAny.Const[Type[Int]](Type(value.width), value)
        }
      }
      implicit def fromTokenW[W0](implicit ctx : DFAny.Context)
      : Aux[TokenW[W0], W0] = new Builder[TokenW[W0]] {
        type W = W0
        def apply(value : TokenW[W0]) : Const[W] = {
          val width = TwoFace.Int.create[W0](value.width)
          DFAny.Const[Type[W0]](Type(width), value)
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends OpCO {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    class AbleOps[L](value : L) extends Able[L](value) {
      final val left = value
      final def |   [RW](right : DFBits[RW])(implicit op: `Op|`.Builder[L, DFBits[RW]]) = op(left, right)
      final def &   [RW](right : DFBits[RW])(implicit op: `Op&`.Builder[L, DFBits[RW]]) = op(left, right)
      final def ^   [RW](right : DFBits[RW])(implicit op: `Op^`.Builder[L, DFBits[RW]]) = op(left, right)
      final def === [RW](right : DFBits[RW])(implicit op: `Op===`.Builder[L, DFBits[RW]]) = op(left, right)
      final def =!= [RW](right : DFBits[RW])(implicit op: `Op=!=`.Builder[L, DFBits[RW]]) = op(left, right)
      final def ++  [RW](right : DFBits[RW])(implicit op: `Op++`.Builder[L, DFBits[RW]]) = op(left, right)
    }
    trait Implicits {
      final implicit def __DFBitsWiden[FW, TW](c : DFBits[FW])(implicit eq : OpContainer.Eq[FW, TW, Int]) : DFBits[TW] = c.asInstanceOf[DFBits[TW]]
      sealed class __DFBitsFromToken(left : Token) extends AbleOps[Token](left)
      final implicit def __DFBitsFromToken(left: Token): __DFBitsFromToken = new __DFBitsFromToken(left)
      sealed class __DFBitsFromTokenW[W](left : TokenW[W]) extends AbleOps[TokenW[W]](left)
      final implicit def __DFBitsFromTokenW[W](left: TokenW[W]): __DFBitsFromTokenW[W] = new __DFBitsFromTokenW[W](left)
      sealed class __DFBitsFromZeros[SBV <: SameBitsVector](left : SBV) extends AbleOps[SBV](left)
      final implicit def __DFBitsFromZeros[SBV <: SameBitsVector](left : SBV) : __DFBitsFromZeros[SBV] = new __DFBitsFromZeros(left)
//      sealed class DFBitsFromDFBool(left : DFBool)(implicit ctx : DFAny.Context) extends AbleOps[DFBits[1]](DFAny.Alias.AsIs(Type(1), left))
//      final implicit def DFBitsFromDFBool(left: DFBool)(implicit ctx : DFAny.Context): DFBitsFromDFBool = new DFBitsFromDFBool(left)
      sealed class __DFBitsFromDefaultRet[W](left : DFAny.DefaultRet[Type[W]])(implicit ctx : DFAny.Context) extends AbleOps[DFBits[W]](left)
      final implicit def __DFBitsFromDefaultRet[W](left : DFAny.DefaultRet[Type[W]])(implicit ctx : DFAny.Context) : __DFBitsFromDefaultRet[W] = new __DFBitsFromDefaultRet(left)
      final implicit def __ofDFBits[W](left : DFBits[W]) : Able[DFBits[W]] = new Able(left)
      final implicit class __DFBitsOps[LW](val left : DFBits[LW]){
        def |   [R](right : Able[R])(implicit op: `Op|`.Builder[DFBits[LW], R]) = op(left, right)
        def &   [R](right : Able[R])(implicit op: `Op&`.Builder[DFBits[LW], R]) = op(left, right)
        def ^   [R](right : Able[R])(implicit op: `Op^`.Builder[DFBits[LW], R]) = op(left, right)
        def === [R](right : Able[R])(implicit op: `Op===`.Builder[DFBits[LW], R]) = op(left, right)
        def =!= [R](right : Able[R])(implicit op: `Op=!=`.Builder[DFBits[LW], R]) = op(left, right)
        def ++  [R](right : Able[R])(implicit op: `Op++`.Builder[DFBits[LW], R]) = op(left, right)
        def unary_~(implicit ctx : DFAny.Context) : DFBits[LW] = DFAny.Alias.Invert(left)
        def << [R](right: DFUInt.Op.Able[R])(implicit op: `Op<<`.Builder[DFBits[LW], R]) = op(left, right)
        def >> [R](right: DFUInt.Op.Able[R])(implicit op: `Op>>`.Builder[DFBits[LW], R]) = op(left, right)
        def resize[RW](toWidth : BitsWidth.Checked[RW])(implicit ctx : DFAny.Context) : DFBits[RW] =
          DFAny.Alias.Resize.bits(left, toWidth)
        def resizeRight[RW](toWidth : BitsWidth.Checked[RW])(implicit ctx : DFAny.Context) : DFBits[RW] = {
          val ret = if (left.width < toWidth) {
            val zeroWidth = toWidth - left.width
            val zeros = DFAny.Const.forced(Type(zeroWidth), Token.zero(zeroWidth))
            `Op++`.forced(left, zeros)
          }
          else if (left.width > toWidth) DFAny.Alias.BitsWL(left, toWidth, left.width - toWidth)
          else left
          ret.asInstanceOf[DFBits[RW]]
        }
      }
      final implicit class __DFBitsAliases[LW, Mod <: DFAny.Modifier](val left : DFAny.Value[Type[LW], Mod]) {
        def uint(implicit ctx : DFAny.Context) : DFAny.Value[DFUInt.Type[LW], Mod] =
          left.as(DFUInt.Type(left.width)).setCodeStringOverride((_, rs) => s"$rs.uint")
        def sint(implicit ctx : DFAny.Context) : DFAny.Value[DFSInt.Type[LW], Mod] =
          left.as(DFSInt.Type(left.width)).setCodeStringOverride((_, rs) => s"$rs.sint")
        def apply[H, L](relBitHigh : BitIndex.Checked[H, left.Width], relBitLow : BitIndex.Checked[L, left.Width])(
          implicit checkHiLow : BitsHiLo.CheckedShell[H, L], relWidth : RelWidth.TF[H, L], ctx : DFAny.Context
        ) : DFAny.Value[DFBits.Type[relWidth.Out], Mod] =
          left.bits(relBitHigh, relBitLow).setCodeStringOverride{(printer, rs) =>
            import printer.config.LIT
            s"$rs($LIT$relBitHigh, $LIT$relBitLow)"
          }
        def apply[I](relBit: BitIndex.Checked[I, left.Width])(
          implicit ctx : DFAny.Context
        ) : DFAny.Value[DFBool.Type, Mod] =
          left.bit(relBit).setCodeStringOverride{(printer, rs) =>
            import printer.config.LIT
            s"$rs($LIT$relBit)"
          }
        def msbit(implicit ctx : DFAny.Context): DFAny.Value[DFBool.Type, Mod] =
          DFAny.Alias.BitsWL.bit(left, left.width-1).setCodeStringOverride((_, rs) => s"$rs.msbit")
        def lsbit(implicit ctx : DFAny.Context): DFAny.Value[DFBool.Type, Mod] =
          DFAny.Alias.BitsWL.bit(left, 0).setCodeStringOverride((_, rs) => s"$rs.lsbit")
      }

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Tuple-handling Implicits
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      sealed abstract class __VarProductExtender(e : Product) {
        type WSum
        protected val wsum : Int = e.productIterator.toList.asInstanceOf[List[DFAny]].map(f => f.width.getValue).sum
        def bits(implicit ctx : DFAny.Context, w : TwoFace.Int.Shell1[Id, WSum, Int]) : DFAny.VarOf[Type[w.Out]] = ???
//          new DFBits.Alias[w.Out](DFAny.Alias.Reference.Concat(e.productIterator.toList.asInstanceOf[List[DFAny]], ".bits"))
      }

      sealed abstract class __ValProductExtender(e : Product) {
        type WSum
        protected val wsum : Int = e.productIterator.toList.collect{
          case dfAny : DFAny => dfAny.width.getValue
          case token : Token => token.width
        }.sum
        def bits(implicit ctx : DFAny.Context, w : TwoFace.Int.Shell1[Id, WSum, Int]) : DFBits[w.Out] = {
          val list : List[DFBits[Int]] = e.productIterator.toList.collect{
            case dfAny : DFAny.Value[_,_] => dfAny.bits.asInstanceOf[DFBits[Int]]
            case token : Token => DFAny.Const.forced(Type(token.width), token)
          }
          list.reduce((l, r) => `Op++`.forced(l, r)).asInstanceOf[DFBits[w.Out]]
        }
      }

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 1
      /////////////////////////////////////////////////////////////////////////////////////
//      implicit class VarTuple1[T1 <: DFAny.Type](
//        val e : Tuple1[DFAny.VarOf[T1]]
//      ) extends VarProductExtender(e) {
//        type WSum = e._1.Width
//      }

      implicit class __ValTuple1[T1 <: HasWidth](
        val e : Tuple1[T1]
      ) extends __ValProductExtender(e){
        type WSum = e._1.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 2
      /////////////////////////////////////////////////////////////////////////////////////
//      implicit class VarTuple2[T1 <: DFAny.Type, T2 <: DFAny.Type](
//        val e : Tuple2[DFAny.VarOf[T1], DFAny.VarOf[T2]]
//      ) extends VarProductExtender(e) {
//        type WSum = e._1.Width + e._2.Width
//      }

      implicit class __ValTuple2[T1 <: HasWidth, T2 <: HasWidth](
        val e : Tuple2[T1, T2]
      ) extends __ValProductExtender(e){
        type WSum = e._1.Width + e._2.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 3
      /////////////////////////////////////////////////////////////////////////////////////
//      implicit class VarTuple3[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type](
//        val e : Tuple3[DFAny.VarOf[T1], DFAny.VarOf[T2], DFAny.VarOf[T3]]
//      ) extends VarProductExtender(e) {
//        type WSum = e._1.Width + e._2.Width + e._3.Width
//      }

      implicit class __ValTuple3[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth](
        val e : Tuple3[T1, T2, T3]
      ) extends __ValProductExtender(e){
        type WSum = e._1.Width + e._2.Width + e._3.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 4
      /////////////////////////////////////////////////////////////////////////////////////
//      implicit class VarTuple4[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type, T4 <: DFAny.Type](
//        val e : Tuple4[DFAny.VarOf[T1], DFAny.VarOf[T2], DFAny.VarOf[T3], DFAny.VarOf[T4]]
//      ) extends VarProductExtender(e) {
//        type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width
//      }

      implicit class __ValTuple4[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth, T4 <: HasWidth](
        val e : Tuple4[T1, T2, T3, T4]
      ) extends __ValProductExtender(e){
        type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 5
      /////////////////////////////////////////////////////////////////////////////////////
//      implicit class VarTuple5[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type, T4 <: DFAny.Type, T5 <: DFAny.Type](
//        val e : Tuple5[DFAny.VarOf[T1], DFAny.VarOf[T2], DFAny.VarOf[T3], DFAny.VarOf[T4], DFAny.VarOf[T5]]
//      ) extends VarProductExtender(e) {
//        type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width + e._5.Width
//      }

      implicit class __ValTuple5[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth, T4 <: HasWidth, T5 <: HasWidth](
        val e : Tuple5[T1, T2, T3, T4, T5]
      ) extends __ValProductExtender(e){
        type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width + e._5.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // SameBitsVector for repeated zeros or ones
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] sealed class SameBitsVector(val value : Boolean)
  object SameBitsVector {
    trait Builder[W] {
      def apply(bits : Type[W], sbv : SameBitsVector) : Const[W]
    }
    object Builder {
      implicit def ev[W](implicit ctx : DFAny.Context)
      : Builder[W] = (bits, sbv) => DFAny.Const[Type[W]](Type(bits.width), Token(XBitVector.fill(bits.width)(sbv.value)))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>` extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable of type ${LType} does not support assignment/connect operation with the type ${R}")
    trait Builder[LType <: DFAny.Type, R] extends DFAny.Op.Builder[LType, R] {
      type Out = DFAny.Of[LType]
    }

    object Builder {
      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "An assignment/connection operation does not permit different widths. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      implicit def evDFBits_op_DFBits[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[Type[LW], DFBits[RW]] = (left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        right.asInstanceOf[DFAny.Of[Type[LW]]]
      }

      implicit def evDFBits_op_SBV[LW, SBV <: SameBitsVector](
        implicit
        ctx : DFAny.Context,
        rSBV : SameBitsVector.Builder[LW]
      ) : Builder[Type[LW], SBV] = (left, right) => {
        rSBV(left, right)
      }

      implicit def evDFBits_op_Const[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[Type[LW], R] = (left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        right.asInstanceOf[DFAny.Of[Type[LW]]]
      }
    }
  }
  object `Op:=` extends `Ops:=,<>`
  object `Op<>` extends `Ops:=,<>`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[Op <: Func2.Op](op : Op)(func : (Token, Token) => DFBool.Token) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}
    object Builder {
      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Comparison operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      def create[L, LW, R, RW](properLR : (L, R) => (DFBits[LW], DFBits[RW]))(
        implicit ctx : DFAny.Context
      ) : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        DFAny.Func2(DFBool.Type(logical = true), left, op, right)(func)
      }

      implicit def evDFBits_op_DFBits[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[DFBits[LW], DFBits[RW]] =
        create[DFBits[LW], LW, DFBits[RW], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

      implicit def evDFBits_op_Const[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[DFBits[LW], R] = create[DFBits[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFBits[L, LW, RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[L, DFBits[RW]] = create[L, LW, DFBits[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFBits_op_SBV[LW, SBV <: SameBitsVector](
        implicit
        ctx : DFAny.Context,
        rSBV : SameBitsVector.Builder[LW]
      ) : Builder[DFBits[LW], SBV] = create[DFBits[LW], LW, SBV, LW]((left, rightSBV) => {
        val right = rSBV(left.dfType, rightSBV)
        (left, right)
      })

      implicit def evSBV_op_DFBits[RW, SBV <: SameBitsVector](
        implicit
        ctx : DFAny.Context,
        lSBV : SameBitsVector.Builder[RW]
      ) : Builder[SBV, DFBits[RW]] = create[SBV, RW, DFBits[RW], RW]((leftSBV, right) => {
        val left = lSBV(right.dfType, leftSBV)
        (left, right)
      })
    }
  }
  object `Op==` extends OpsCompare(Func2.Op.==)((l, r) => l == r) with `Op==`
  object `Op!=` extends OpsCompare(Func2.Op.!=)((l, r) => l != r) with `Op!=`
  object `Op===` extends OpsCompare(Func2.Op.==)((l, r) => l == r)
  object `Op=!=` extends OpsCompare(Func2.Op.!=)((l, r) => l != r)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Logic operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsLogic[Op <: Func2.Op](op : Op)(tokenOp : (Token, Token) => Token) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Logic Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Out = Comp0
      }

      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Logic operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      trait DetailedBuilder[L, LW, R, RW] {
        type Out
        def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, R, RW](
          implicit
          ctx : DFAny.Context,
          checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
        ) : DetailedBuilder[L, LW, R, RW]{type Out = DFBits[LW]} =
          new DetailedBuilder[L, LW, R, RW]{
            type Out = DFBits[LW]
            def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out] =
              new Builder[L, R] {
                type Out = DFBits[LW]
                def apply(leftL : L, rightR : R) : Out = {
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  DFAny.Func2[Type[LW], DFBits[LW], Op, DFBits[RW]](Type[LW](left.width), left, op, right)(tokenOp)
                }
              }
          }
      }

      implicit def evDFBits_op_DFBits[L <: DFBits[LW], LW, R <: DFBits[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, DFBits[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFBits_op_Const[L <: DFBits[LW], LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFBits[L, LW, LE, R <: DFBits[RW], RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        detailedBuilder: DetailedBuilder[L, LW, DFBits[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))

      type UnconstrainedLiteralError =
        RequireMsg[false, "An unconstrained-width literal cannot be used in a logic operation"]

      implicit def evDFBits_op_SBV[LW, SBV <: SameBitsVector](implicit error : UnconstrainedLiteralError)
      : Aux[DFBits[LW], SBV, DFBits[LW]] = ???
      implicit def evSBV_op_DFBits[RW, SBV <: SameBitsVector](implicit error : UnconstrainedLiteralError)
      : Aux[SBV, DFBits[RW], DFBits[RW]] = ???
    }
  }
  object `Op|` extends OpsLogic(Func2.Op.|)(_ | _)
  object `Op&` extends OpsLogic(Func2.Op.&)(_ & _)
  object `Op^` extends OpsLogic(Func2.Op.^)(_ ^ _)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Shift operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op<<` extends OpsShift[Type](Func2.Op.<<) {
    def tokenFunc[LW](left: DFBits.Token, right: DFUInt.Token) : DFBits.Token = left << right
  }
  object `Op>>` extends OpsShift[Type](Func2.Op.>>) {
    def tokenFunc[LW](left: DFBits.Token, right: DFUInt.Token) : DFBits.Token = left >> right
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Concatenation operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op++` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support a Concatenation Op with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    def forced[LW, RW](left : DFBits[LW], right : DFBits[RW])(implicit ctx : DFAny.Context) : DFBits[Int] =
      DFAny.Func2(Type(left.width.getValue + right.width.getValue), left, DFAny.Func2.Op.++, right)(_ ++ _).asInstanceOf[DFBits[Int]]
    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Out = Comp0
      }

      object Inference {
        type CalcW[LW, RW] = LW + RW
        type OW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, R, RW] {
        type Out
        def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, R, RW, OW](
          implicit
          ctx : DFAny.Context,
          oW : Inference.OW[LW, RW, OW],
        ) : DetailedBuilder[L, LW, R, RW]{type Out = DFBits[OW]} =
          new DetailedBuilder[L, LW, R, RW]{
            type Out = DFBits[OW]
            def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out] =
              new Builder[L, R] {
                type Out = DFBits[OW]
                def apply(leftL : L, rightR : R) : Out = {
                  val (left, right) = properLR(leftL, rightR)
                  // Constructing op
                  val oWidth = oW(left.width, right.width)
                  val out = DFAny.Func2(Type(oWidth), left, DFAny.Func2.Op.++, right)(_ ++ _)
                  out
                }
              }
          }
      }

      implicit def evDFBits_op_DFBits[L <: DFBits[LW], LW, R <: DFBits[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, DFBits[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFBits_op_Const[L <: DFBits[LW], LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFBits[L, LW, LE, R <: DFBits[RW], RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        detailedBuilder: DetailedBuilder[L, LW, DFBits[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))

      type UnconstrainedLiteralError =
        RequireMsgSym[false, "An unconstrained-width literal cannot be used in a concatenation operation", Builder[_,_]]

      implicit def evDFBits_op_SBV[LW](implicit error : UnconstrainedLiteralError)
      : Aux[DFBits[LW], SameBitsVector, DFBits[LW]] = ???
      implicit def evSBV_op_DFBits[RW](implicit error : UnconstrainedLiteralError)
      : Aux[SameBitsVector, DFBits[RW], DFBits[RW]] = ???
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}