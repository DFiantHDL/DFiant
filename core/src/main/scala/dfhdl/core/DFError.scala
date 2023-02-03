package dfhdl.core

final class DFError(
    val dfMsg: String
) extends Exception(dfMsg)
    derives CanEqual
