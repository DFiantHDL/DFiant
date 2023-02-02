package AES

import dfhdl.*
abstract class AESMatrix[C <: Int with Singleton](val colNum: C)
    extends core.DFOpaque.Frontend2(Boolean)(colNum)
