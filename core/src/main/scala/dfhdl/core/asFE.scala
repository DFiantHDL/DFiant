package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir

export DFType.asFE

given canEqualNothingL: CanEqual[Nothing, Any] = CanEqual.derived
given canEqualNothingR: CanEqual[Any, Nothing] = CanEqual.derived
