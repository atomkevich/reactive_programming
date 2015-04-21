object aa {
  val num = Var(1)
  val twice = Signal(num() * 2)
  num() = 2
}