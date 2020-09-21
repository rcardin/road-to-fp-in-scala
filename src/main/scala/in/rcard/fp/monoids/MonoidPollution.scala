package in.rcard.fp.monoids

import cats.{Foldable, Monoid}
import cats.kernel.Semigroup
import in.rcard.fp.semigroups.Pollution.Pollution

object MonoidPollution {

  val pollutions: List[Pollution] = List(Pollution(34.2D), Pollution(56.7))
  val pollution: Pollution = Pollution(12.3)

  import cats.syntax.semigroup._
  def fold[P: Semigroup](list: List[P], empty: P): P = {
    list.foldLeft(empty) {
      case (sum, elem) => sum |+| elem
    }
  }

  val sumOfPollutions: Pollution = fold(pollutions, Pollution(0.0D))

  println(s"Sum of all the pollutions $sumOfPollutions)")

  implicit val pollutionMonoid: Monoid[Pollution] = new Monoid[Pollution] {
    override def empty: Pollution = Pollution(0.0D)

    override def combine(x: Pollution, y: Pollution): Pollution =
      Pollution(x.dailyCO2Grams + y.dailyCO2Grams)
  }

  def foldM[P: Monoid](list: List[P]): P = list.foldLeft(Monoid[P].empty) {
    case (sum, elem) => sum |+| elem
  }

  import cats.instances.list._

  val sumOfPollutionUsingFoldable: Pollution = Foldable[List].fold(pollutions)

  println(s"Sum of all the pollutions $sumOfPollutionUsingFoldable")
}
