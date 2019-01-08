package fr.upem.partiel.part1

import java.time.Instant
import java.time.temporal.ChronoUnit.YEARS



// Part 1 (10pts)
object Part1 {

  // 1.1 Apply 'mul2' using pattern matching to the given integer (.5pts)
  def mul2(i: Int): Int = i * 2
  def applyMul2WithPatternMatching(i: Option[Int]): Option[Int] = {
    i match {
      case None => None
      case Some(value) => Some(value*2)
    }
  }

  def applyMul2WithoutPatternMatching(i: Option[Int]): Option[Int] = {
    i.map(x => x*2)
  }

  // 1.3 Refactor the following code using pattern matching (1pts)
  sealed trait Animal
  case object Cat extends Animal
  case object Bird extends Animal
  case class Dog(age: Int) extends Animal

  def formatAnimal(animal: Animal): String = animal match{
    case Cat => "It's a cat"
    case Bird => "It's a bird"
    case Dog(a) =>  s"It's a ${a} year old dog"
    case _ => throw new RuntimeException("This should not happen but I'm a Java developer !")

  }


  // 1.4 Find the index of the given element if any, use recursivity (1pts)
  def indexOf[A](l: List[A], a: A): Option[Int] = l match {
    case x :: xs if x == a => Option(0)
    case x :: Nil if x != a => None
    case x :: xs => indexOf(xs, a) match {
      case Some(value) => Option(value + 1)
      case None => None
    }
    case Nil => None
  }

  // 1.5 Throw away all errors (.5pts)
  case class Error(message: String)

  def keepValid[A](l: List[Either[Error, A]]): List[A] = l match {
    case head::tail => head match{
      case Left(b) => keepValid(tail)
      case Right(b) => b::keepValid(tail)
    }
    case Nil => Nil
  }

  // 1.6 Aggregate values (.5pts)
  def aggregate[A](l: List[A], combine: (A, A) => A, empty: A): A =
    l match {
      case Nil => empty
      case h :: tail => combine(h, aggregate(tail, combine, empty))
    }

  // 1.7 Aggregate valid values (.5pts)
  def aggregateValid[A](l: List[Either[Error, A]], combine: (A, A) => A, empty: A): A =
    aggregate(keepValid(l), combine, empty)

  trait Monoid[A]{
    def value:A
    def operate(a:A, b:A):A

  }
  def aggregateValidM[A](l:List[Either[Error, Monoid[A]]],empty: A) : A = {
    def function(l: List[Monoid[A]], empty: A) : A = l match {
      case head :: tail => head.operate(head.value, function(tail,empty))
      case Nil => empty
    }
    function(keepValid(l),empty)
  }


  // 1.9 Implement the Monoid typeclass for Strings and give an example usage with aggregateValidM (.5pts)
  object Monoid {
    implicit val monoidString = new Monoid[String] {
      def value = ""

      def operate(x: String, y: String): String = x + y
    }
  }
  val mono = Monoid.monoidString("Wesh Alors")
  aggregateValidM(List(mono, mono), "")


  // 1.10 Refactor the following object oriented hierarchy with an ADT (1.5pts)
  // Pas le temps de faire quelque chose de bien
  sealed trait FinancialAsset {
    def computeEarnings: Double
  }

  case class FlatRateAsset(rate: Double, amount: Double) extends FinancialAsset {
    override def computeEarnings: Double = amount + (amount * rate)
  }

  object LivretA {
    private val Rate: Double = 0.75
  }

  case class LivretA(override val amount: Double) extends FlatRateAsset(LivretA.Rate, amount) {
  }

  object Pel {
    private val Rate: Double = 1.5
    private val GovernmentGrant: Int = 1525
  }

  case class Pel(override val amount: Double, creation: Instant) extends FlatRateAsset(Pel.Rate, amount) {
    override protected val rate: Double = Pel.Rate
    override def computeEarnings: Double =
      if (Instant.now().minus(4, YEARS).isAfter(creation))
        super.computeEarnings + Pel.GovernmentGrant
      else
        super.computeEarnings
  }

  object CarSale {
    private val StateHorsePowerTaxation: Int = 500
  }

  class CarSale(amount: Int, horsePower: Int) extends FinancialAsset {
    override def computeEarnings: Double = amount - (CarSale.StateHorsePowerTaxation * horsePower)
  }
  
  // 1.11 Extract the "computeEarnings" logic of the above hierarchy
  // into an "Earnings" typeclass and create the adequate instances (1.5pts)

  // 1.12 Rewrite the following function with your typeclass (.5pts)
  def computeTotalEarnings(assets: List[FinancialAsset]): Double =
    assets.map(_.computeEarnings).sum


  // 1.13 Enrich the "String" type with an "atoi" extension method that parses the
  // given String to an Int IF possible (1pts)

  sealed trait MyReturn // ou Either

  final case class MyInteger(a: Integer) extends MyReturn
  final case class MyString(a: String) extends MyReturn

  implicit class StringPlus(val a: String) extends AnyVal {
    def atoi: MyReturn = if (a.matches("[0-9]+$")) MyInteger(Integer.parseInt(a)) else MyString(a)
  }


}
