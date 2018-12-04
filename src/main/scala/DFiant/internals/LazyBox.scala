package DFiant.internals

import DFiant.internals.LazyBox.ValueOrError

import scala.collection.mutable

abstract class LazyBox[+T] private (args : List[LazyBox[_]])(implicit n : NameIt) {
  import LazyBox.ValueOrError._
  def valueFunc : ValueOrError[T]
  val owner : DSLMemberConstruct
  final val name : String = n.value
  lazy val typeName: String = s"LazyBox.${getClass.getSimpleName}"
  private var visitedCnt : Int = 0
  private var locked : Boolean = false
  private[this] var valueOrError : ValueOrError[T] = Error(List(this), "Uninitialized")
  private def getUpdatedValueOrError : ValueOrError[T] = try {
    valueFunc
  } catch  {
    case e : Exception =>
      Error(List(this), s"Exception occured when calculating LazyBox value: ${e.getMessage}")
  }
  private val valueDependencies : mutable.Set[LazyBox[_]] = mutable.Set.empty[LazyBox[_]]
  final def getDependencyNum : Int = valueDependencies.size
  final protected def unlockValueDependencies() : Unit = if (locked) {
    locked = false
    valueDependencies.foreach(vd => vd.unlockValueDependencies())
  }
  final protected def addValueDependency(lb : LazyBox[_]) : Unit = valueDependencies += lb
  final protected def clearValueDependencies() : Unit = valueDependencies.clear()
  private def getCircularError : ValueOrError[T] = Error(List(this), "Circular dependency detected")
  protected def getFallBackValue : ValueOrError[T] = getUpdatedValueOrError
  var checkFallBack : Boolean = false
  final def getValueOrError : ValueOrError[T] = {
    if (!locked) {
      visitedCnt += 1
      valueOrError = visitedCnt match {
        case 1 =>
          val updatedValueOrError = getUpdatedValueOrError
          (updatedValueOrError, valueOrError) match {
            case (Value(vNew), Value(vOld)) if vNew != vOld && checkFallBack =>
              checkFallBack = false
              Error(List(this), "Contradiction in circular dependency")
            case _ => updatedValueOrError
          }
        case 2 =>
          checkFallBack = true
          getFallBackValue
        case _ => getCircularError
      }
//      if (visitedCnt == 1 && name == "pipeLB" && !owner.asInstanceOf[DFiant.DFAny].isAnonymous)
//        println(f"${owner.fullName + s".$name"}%-60s $valueOrError")

      visitedCnt -= 1
      locked = true
    }
    valueOrError
  }
  def clearValue() : Unit = {
    valueOrError = Error(List(this),"Uninitialized")
    unlockValueDependencies()
  }
  final def get : T = getValueOrError match {
    case Value(v) => v
    case Error(p, m) =>
      val pStr = p.map(lb => s"${lb.owner.fullName}.${lb.name} : ${lb.typeName}").mkString(" <- ")
      throw new IllegalArgumentException(s"\n$m at $pStr")
  }
  args.foreach(a => a.addValueDependency(this))
}


object LazyBox {
  sealed trait ValueOrError[+T]
  object ValueOrError {
    case class Value[+T](value : T) extends ValueOrError[T]
    case class Error(path : List[LazyBox[_]], msg : String) extends ValueOrError[Nothing]
  }
  //cdFallBack - in case of circular dependency, fallback to the initialization value
  case class Mutable[T](owner : DSLMemberConstruct)(initialization : Option[T] = None, cdFallBack : Boolean = false)(implicit n : NameIt) extends LazyBox[T](List()){
    import LazyBox.ValueOrError._
    private var mutableValueFunc : () => ValueOrError[T] = () => initialization match {
      case Some(t) => Value(t)
      case _ => Error(List(this), "Uninitialized")
    }
    override protected def getFallBackValue : ValueOrError[T] =
      if (cdFallBack && initialization.isDefined)  Value(initialization.get)
      else super.getFallBackValue
    final def valueFunc : ValueOrError[T] = mutableValueFunc() match {
      case Error(p, m) => Error(this :: p, m)
      case Value(v) => Value(v)
    }
    private var isset = false
    final def isSet : Boolean = isset
    def set(value : LazyBox[T]) : Unit = {
      clearValueDependencies()
      addValueDependency(value)
      unlockValueDependencies()
      mutableValueFunc = () => value.getValueOrError
      isset = true
    }
    def set(value : T) : Unit = set(Const(owner)(value))
  }
  case class Const[+T](owner : DSLMemberConstruct)(value : => T)(implicit n : NameIt) extends LazyBox[T](List()){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = Value(value)
  }
  case class Args1[+T, +A](owner : DSLMemberConstruct)(func : A => T, arg : LazyBox[A])(implicit n : NameIt) extends LazyBox[T](List(arg)){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = arg.getValueOrError match {
      case Error(p, m) => Error(this :: p, m)
      case Value(a) => Value(func(a))
    }
  }
  case class Args1C[+T, +A, C](owner : DSLMemberConstruct)(func : (A, C) => T, arg : LazyBox[A], const : => C)(implicit n : NameIt) extends LazyBox[T](List(arg)){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = arg.getValueOrError match {
      case Error(p, m) => Error(this :: p, m)
      case Value(a) => Value(func(a, const))
    }
  }
  case class Args2[+T, +L, +R](owner : DSLMemberConstruct)(func : (L, R) => T, leftArg : LazyBox[L], rightArg : LazyBox[R])(implicit n : NameIt) extends LazyBox[T](List(leftArg, rightArg)){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = (leftArg.getValueOrError, rightArg.getValueOrError) match {
      case (Error(p, m), _) => Error(this :: p, m)
      case (_, Error(p, m)) => Error(this :: p, m)
      case (Value(l), Value(r)) => Value(func(l, r))
    }
  }
  case class Args3[+T, +A1, +A2, +A3](owner : DSLMemberConstruct)(func : (A1, A2, A3) => T, arg1 : LazyBox[A1], arg2 : LazyBox[A2], arg3 : LazyBox[A3])(implicit n : NameIt) extends LazyBox[T](List(arg1, arg2, arg3)){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = (arg1.getValueOrError, arg2.getValueOrError, arg3.getValueOrError) match {
      case (Error(p, m), _, _) => Error(this :: p, m)
      case (_, Error(p, m), _) => Error(this :: p, m)
      case (_, _, Error(p, m)) => Error(this :: p, m)
      case (Value(a1), Value(a2), Value(a3)) => Value(func(a1, a2, a3))
    }
  }
  case class Args1List[+T, +A, +L](owner : DSLMemberConstruct)(func : (A, List[L]) => T, arg : LazyBox[A], argList : List[LazyBox[L]])(implicit n : NameIt) extends LazyBox[T](arg :: argList){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = arg.getValueOrError match {
      case Error(p, m) => Error(this :: p, m)
      case Value(argVal) =>
        val xs = argList.map(a => a.getValueOrError)
        xs collectFirst {case x : Error => x} getOrElse Value(func(argVal, xs.collect {case Value(x) => x}))
    }
  }
  case class ArgList[+T, +L](owner : DSLMemberConstruct)(func : List[L] => T, argList : List[LazyBox[L]])(implicit n : NameIt) extends LazyBox[T](argList){
    import LazyBox.ValueOrError._
    final def valueFunc : ValueOrError[T] = {
      val xs = argList.map(a => a.getValueOrError)
      xs collectFirst {case x : Error => x} getOrElse Value(func(xs.collect {case Value(x) => x}))
    }
  }
}

