package in.rcard.fp.typeclasses

import in.rcard.fp.typeclasses.CarbonFootPrint.MethodOverloading.{carbonFootPrint, frisona, me, superCar}

object CarbonFootPrint {

  case class Cow(weight: Double)
  case class Car(hp: Int)
  case class Human(age: Int)

  trait Data {
    val frisona: Cow = Cow(700.0D)
    val superCar: Car = Car(800)
    val me: Human = Human(38)
  }

  /**
   * Every time we add a new type we need to overload the function carbonFootPrint :(
   */
  object MethodOverloading extends Data {
    def carbonFootPrint(cow: Cow): Double = cow.weight * 8
    def carbonFootPrint(car: Car): Double = car.hp * 3.4D
    def carbonFootPrint(human: Human): Double = human.age * 15.5D

    println(s"The carbon footprint of a frisona cow is ${carbonFootPrint(frisona)}")
    println(s"The carbon footprint of a super car is ${carbonFootPrint(superCar)}")
    println(s"My carbon footprint is ${carbonFootPrint(me)}")
  }

  trait Pollutant[T] {
    def pollute(p: T): Double
  }

  object Implicits {
    implicit val cowPollute: Pollutant[Cow] = (cow: Cow) => cow.weight * 8
    implicit val carPollute: Pollutant[Car] = (car: Car) => car.hp * 3.4D
    implicit val humanPollute: Pollutant[Human] = (human: Human) => human.age * 15.5D
  }

  object AdHocPolymorphism extends Data {
    import Implicits._
    def carbonFootPrint[T](obj: T)(implicit pollutant: Pollutant[T]): Double =
      pollutant.pollute(obj)

    def carbonFootPrintAltSyntax[T: Pollutant](obj: T): Double =
      implicitly[Pollutant[T]].pollute(obj)

    println(s"The carbon footprint of a frisona cow is ${carbonFootPrintAltSyntax(frisona)}")
    println(s"The carbon footprint of a super car is ${carbonFootPrintAltSyntax(superCar)}")
    println(s"My carbon footprint is ${carbonFootPrintAltSyntax(me)}")
  }
}
