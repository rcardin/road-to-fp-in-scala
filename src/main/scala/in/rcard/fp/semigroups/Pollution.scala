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
}
