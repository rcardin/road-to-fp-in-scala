package in.rcard.fp.semigroups

object Pollution {

  case class Pollution(dailyCO2Grams: Double)

  trait Data {
    val cowPollution: Pollution = Pollution(100.0D)
    val superCarPollution: Pollution = Pollution(56.4D)
    val pollutions: Map[String, Pollution] = Map(
      "frisona" -> Pollution(78.2D),
      "bmw" -> Pollution(340.7D)
    )
  }
}
