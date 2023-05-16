object IDTopApp extends App {
  import dfhdl.compiler.backend.verilog.v2001
  val idTop = new IDTop
  idTop.compile.printGenFiles(colored = false)
}
