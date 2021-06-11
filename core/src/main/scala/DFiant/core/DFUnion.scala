package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.quoted.*
import collection.immutable.ListSet
import DFiant.compiler.printing.DefaultPrinter

private def collisionError(collisions: List[String]): String =
  s"Dataflow union types must be exclusive.\nThe following types are repeated: ${collisions.mkString(", ")}"
private def widthError(lhsWidth: Int, rhsWidth: Int): String =
  s"All union types must have the same width.\nFound LHS-width $lhsWidth and RHS-width $rhsWidth"
opaque type DFUnion[U <: DFType] <: ir.DFUnion = ir.DFUnion
object DFUnion:
  def apply[U <: DFType](fieldSet: ListSet[DFType]): DFUnion[U] =
    ir.DFUnion(fieldSet)
  trait Able[T]:
    type U <: DFType
    def apply(t: T): DFUnion[U]
  object Able:
    given fromDFType[T <: DFType]: Able[T] with
      type U = T
      def apply(t: T): DFUnion[U] = DFUnion[U](ListSet(t))
    given fromFields[T](using tc: DFType.TC[T]): Able[T] with
      type U = tc.Type
      def apply(t: T): DFUnion[U] = DFUnion[U](ListSet(tc(t)))
    given fromUnion[U0 <: DFType]: Able[DFUnion[U0]] with
      type U = U0
      def apply(t: DFUnion[U0]): DFUnion[U] = t
  object Ops:
    extension [L](lhs: L)(using l: Able[L])
      def |[R](rhs: R)(using r: Able[R])(using
          VerifyUnion[l.U, r.U]
      ): DFUnion[l.U | r.U] =
        val lhsUnion = l(lhs)
        val rhsUnion = r(rhs)
        val collisions = lhsUnion.fieldSet & rhsUnion.fieldSet
        if (collisions.nonEmpty)
          throw new IllegalArgumentException(
            collisionError(
              collisions.toList.map(_.codeString(using DefaultPrinter))
            )
          )
        assert(
          lhsUnion.__width == rhsUnion.__width,
          widthError(lhsUnion.__width, rhsUnion.__width)
        )
        DFUnion[l.U | r.U](lhsUnion.fieldSet ++ rhsUnion.fieldSet)

trait VerifyUnion[Current <: DFType, Added <: DFType]
object VerifyUnion:
  inline given [Current <: DFType, Added <: DFType]
      : VerifyUnion[Current, Added] =
    ${ verifyMacro[Current, Added] }
  def verifyMacro[Current <: DFType, Added <: DFType](using
      Quotes,
      Type[Current],
      Type[Added]
  ): Expr[VerifyUnion[Current, Added]] =
    import quotes.reflect.*
    //flattens all the OrType into a list
    def flattenOr(tpe: TypeRepr): List[TypeRepr] =
      tpe.dealias match
        case OrType(left, right) => flattenOr(left) ++ flattenOr(right)
        case t                   => List(t)
    val currentTpes = flattenOr(TypeRepr.of[Current])
    val addedTpes = flattenOr(TypeRepr.of[Added])
    //checking for collisions
    val collisions =
      currentTpes.filter(c => addedTpes.exists(a => a <:< c | c <:< a))
    if (collisions.nonEmpty)
      report.error(collisionError(collisions.map(_.toString)))
    import Width.*
    //checking widths match
    val currentWidth = currentTpes.head.calcWidth
    val addedWidth = addedTpes.head.calcWidth
    (currentWidth, addedWidth) match
      case (ConstantType(IntConstant(c)), ConstantType(IntConstant(a)))
          if a != c =>
        report.error(widthError(c, a))
      //either unknown at compile-time or are matching
      case _ =>

    '{ new VerifyUnion[Current, Added] {} }
