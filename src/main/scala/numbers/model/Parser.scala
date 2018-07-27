package numbers.model

trait Parser[I, A] {
  def parse(input: I): Option[A]
}
