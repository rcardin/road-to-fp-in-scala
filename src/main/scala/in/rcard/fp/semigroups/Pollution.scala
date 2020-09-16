package in.rcard.fp.semigroups

import in.rcard.fp.semigroups.Pollution.ImplicitsGenericAddExample.{add, monthlyPollutions, pollutions}

object Pollution {

  case class Pollution(dailyCO2Grams: Double)

  trait Data {
    val cowPollution: Pollution = Pollution(100.0D)
    val superCarPollution: Pollution = Pollution(56.4D)
    val pollutions: Map[String, Pollution] = Map(
      "frisona" -> Pollution(78.2D),
      "bmw" -> Pollution(340.7D)
    )
    val monthlyPollutions: Map[String, Pollution] = Map(
      "frisona" -> Pollution(1234.2D),
      "me" -> Pollution(12000.7D)
    )
  }

  object SimpleAddExample extends Data {
    def add(p1: Pollution, p2: Pollution): Pollution =
      Pollution(p1.dailyCO2Grams + p2.dailyCO2Grams)

    def addPollutionMap(monthlyPollutions: Map[String, Pollution], dailyPollutions: Map[String, Pollution]): Map[String, Pollution] = {
      monthlyPollutions.foldLeft(dailyPollutions) {
        case (acc, (name, pollution)) =>
          acc + (name -> acc.get(name).map(add(_, pollution)).getOrElse(pollution))
      }
    }

    println(s"Add two pollutions ${add(cowPollution, superCarPollution)}")
    println(s"Add two pollutions maps ${addPollutionMap(pollutions, monthlyPollutions)}")
  }

  object GenericAddExample extends Data {

    trait Addable[T] {
      def add(a: T, b: T): T
    }

    def addWithAddable[K, V](pollutions: Map[K, V], toAdd: Map[K, V])(addable: Addable[V]): Map[K, V] = {
      toAdd.foldLeft(pollutions) {
        case (acc, (name, pollution)) =>
          acc + (name -> acc.get(name).map(addable.add(_, pollution)).getOrElse(pollution))
      }
    }

    import SimpleAddExample.add

    println(s"Add two pollutions maps ${
      addWithAddable(pollutions, monthlyPollutions) { (p1: Pollution, p2: Pollution) => add(p1, p2) }
    }")
  }

  object ImplicitsGenericAddExample extends Data {

    import GenericAddExample.Addable

    implicit val pollutionAdd: Addable[Pollution] = SimpleAddExample.add

    implicit def mapsAdd[K, V: Addable]: Addable[Map[K, V]] =
      (a: Map[K, V], b: Map[K, V]) => a.foldLeft(b) {
        case (acc, (key, value)) =>
          acc + (key -> acc.get(key).map(implicitly[Addable[V]].add(_, value)).getOrElse(value))
      }

    def add[A: Addable](a1: A, a2: A): A = implicitly[Addable[A]].add(a1, a2)

    println(s"Add two pollutions maps ${add(pollutions, monthlyPollutions)}")
  }

  object CatsGenericAddBalanceExample extends Data {

    import cats.Semigroup

    implicit val pollutionSemigroup: Semigroup[Pollution] =
      (x: Pollution, y: Pollution) => Pollution(x.dailyCO2Grams + y.dailyCO2Grams)

    def add[A: Semigroup](a1: A, a2: A): A = implicitly[Semigroup[A]].combine(a1, a2)

    import cats.instances.map._

    println(s"Add two pollutions maps ${add(pollutions, monthlyPollutions)}")
  }

}
