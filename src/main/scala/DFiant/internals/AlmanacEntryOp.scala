package DFiant.internals


///////////////////////////////////////////////////////////////////////////////////////
//(:=) Identity assignment
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryAssign private (arg0 : AlmanacEntry, arg1 : AlmanacEntry)(implicit almanacID : AlmanacID = arg0.id, almanacAddress : AlmanacAddress = Almanac.getCurrentAddress, bitsRange : BitsRange = arg0.bitsRange) extends AlmanacEntry {
  override def toString: String = s"$arg0 := $arg1"
  if (Almanac.printEntrees) {
    println(this)
  }
}
object AlmanacEntryAssign {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryAssign(arg0, arg1))
}

//Set Operation Entry. Used for an
abstract class AlmanacEntryOp(implicit almanacID : AlmanacID = AlmanacID(), almanacAddress : AlmanacAddress = AlmanacAddressLatest, bitsRange : BitsRange) extends AlmanacEntry() {
  def opString : String
  if (Almanac.printEntrees) {
    println(this)
  }
}

abstract class AlmanacEntryOp1(val arg0 : AlmanacEntry)(implicit bitsRange : BitsRange) extends AlmanacEntryOp {
  override def toString: String = s"${super.toString} := $opString$arg0"
}

///////////////////////////////////////////////////////////////////////////////////////
//(!, ~) Invert (Not)
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryOpInv private (arg0 : AlmanacEntry)(implicit bitsRange : BitsRange = arg0.bitsRange) extends AlmanacEntryOp1(arg0) {
  def opString : String = "~"
}
object AlmanacEntryOpInv {
  def apply(arg0 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpInv(arg0))
}

///////////////////////////////////////////////////////////////////////////////////////
//(-) Negate (as a prefix operator)
///////////////////////////////////////////////////////////////////////////////////////
//TBD. May be changed to (0 - arg)
class AlmanacEntryOpNeg private (arg0 : AlmanacEntry)(implicit bitsRange : BitsRange = arg0.bitsRange) extends AlmanacEntryOp1(arg0) {
  def opString : String = "-"
}
object AlmanacEntryOpNeg {
  def apply(arg0 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpNeg(arg0))
}


abstract class AlmanacEntryOp2(val arg0 : AlmanacEntry, val arg1 : AlmanacEntry)
(implicit bitsRange : BitsRange = arg0.bitsRange.max(arg1.bitsRange)) extends AlmanacEntryOp {
  override def toString: String = s"${super.toString} := $arg0 $opString $arg1"
}

///////////////////////////////////////////////////////////////////////////////////////
//(^) XOR
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryOpXor private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
  def opString : String = "^"
}
object AlmanacEntryOpXor {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpXor(arg0, arg1))
}

///////////////////////////////////////////////////////////////////////////////////////
//(|, ||) OR
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryOpOr private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
  def opString : String = "|"
}
object AlmanacEntryOpOr {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpOr(arg0, arg1))
}

///////////////////////////////////////////////////////////////////////////////////////
//(&, &&) And
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryOpAnd private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
  def opString : String = "&"
}
object AlmanacEntryOpAnd {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpAnd(arg0, arg1))
}

///////////////////////////////////////////////////////////////////////////////////////
//(##) Concatenation
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryOpCat private (arg0 : AlmanacEntry, arg1 : AlmanacEntry)(implicit bitsRange : BitsRange = arg0.bitsRange + arg1.bitsRange) extends AlmanacEntryOp2(arg0, arg1) {
  def opString : String = "##"
}
object AlmanacEntryOpCat {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpCat(arg0, arg1))
}

///////////////////////////////////////////////////////////////////////////////////////
//(<<) Left Shift
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryOpLsh private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
  def opString : String = "<<"
}
object AlmanacEntryOpLsh {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpLsh(arg0, arg1))
}

///////////////////////////////////////////////////////////////////////////////////////
//(>>) Right Shift
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryOpRsh private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
  def opString : String = ">>"
}
object AlmanacEntryOpRsh {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpRsh(arg0, arg1))
}

///////////////////////////////////////////////////////////////////////////////////////
//(+) Add
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryOpAdd private (arg0 : AlmanacEntry, arg1 : AlmanacEntry)
(implicit bitsRange : BitsRange = arg0.bitsRange.max(arg1.bitsRange).incBy(1)) extends AlmanacEntryOp2(arg0, arg1) {
  def opString : String = "+"
}
object AlmanacEntryOpAdd {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpAdd(arg0, arg1))
}

///////////////////////////////////////////////////////////////////////////////////////
//(-) Subtract
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryOpSub private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
  def opString : String = "-"
}
object AlmanacEntryOpSub {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpSub(arg0, arg1))
}

///////////////////////////////////////////////////////////////////////////////////////
//(==) Equals
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryOpEq private (arg0 : AlmanacEntry, arg1 : AlmanacEntry)(override implicit val bitsRange : BitsRange = BitsRange(0,0)) extends AlmanacEntryOp2(arg0, arg1) {
  def opString : String = "=="
}
object AlmanacEntryOpEq {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpEq(arg0, arg1))
}

///////////////////////////////////////////////////////////////////////////////////////
//(<) Less Than
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryOpLsTn private (arg0 : AlmanacEntry, arg1 : AlmanacEntry)(override implicit val bitsRange : BitsRange = BitsRange(0,0)) extends AlmanacEntryOp2(arg0, arg1) {
  def opString : String = "<"
}
object AlmanacEntryOpLsTn {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpLsTn(arg0, arg1))
}



//TBD
trait AlmanacTypeIO extends AlmanacEntry {

}

