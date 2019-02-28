// Copyright(C) 2019 - John A. De Goes. All rights reserved.

package net.degoes.zio
package essentials

import java.time.LocalDate

import net.degoes.zio.essentials.functions.Command._

import scala.util.Try

/**
 * Functions are total, deterministic, and free of side effects.
 * Pure functions should be:
 *     Total
 *     Deterministic
 *     Free of side effect
  *
  *     Domain = { John, Bob, Sarah }
  *     Codomain = { Hungry, Content, Full }
  *     f(p: Domain): Codomain
  *
  *
  *
 */
object functions {

  /**
   * Turn the following pseudo-functions into functions.
   */
  /**
   * Partial => Total
   */
  def parseInt1(s: String): Int   = s.toInt
  def parseInt2(s: String): Try[Int] = Try(s.toInt)

  def divide1(a: Int, b: Int): Int = a / b
  def divide2(a: Int, b: Int): Option[Int] =
    b match {
      case 0 =>
        None
      case x =>
        Some(a / x)
    }

  def head1[A](l: Seq[A]): A     = l.head
  def head2[A](l: Seq[A]): Option[A] =
    l.headOption

  def secondChar1(str: String): Char = str.charAt(2)
  def secondChar2(str: String): Option[Char] =
    if (str.length > 2)
      Some(str.charAt(2))
    else None

  /**
   * Non-deterministic => Deterministic
   */
  def increment1: Int            = scala.util.Random.nextInt(0) + 1
  def increment2(base: Int): Int = base + 1

  def nextDay1: LocalDate        = LocalDate.now.plusDays(1)
  def nextDay2(currentDay: LocalDate): LocalDate = currentDay.plusDays(1)

  case object IncorrectAge extends Exception
  def computeAge1(year: Int): Int = {
    val age = LocalDate.now.getYear - year
    if (age < 0) throw IncorrectAge
    else age
  }
  def computeAge2(now: LocalDate, year: Int): Int =
    now.getYear - year

  /**
   * Side effects => Free of side effects
   */
  def get1(a: Int): Int = {
    println(s"the given of a is: $a")
    a
  }
  def get2(a: Int): Int = a

  def sumN1(n: Int): Int = {
    var result = 0
    (1 to n).foreach(i => result = result + i)
    result
  }

  def updateArray1[A](arr: Array[A], i: Int, f: A => A): Unit =
    arr.update(i, f(arr(i)))
  def updateArray2[A](arr: List[A], i: Int, f: A => A): Option[List[A]] =
    if (arr.size > i)
      Some(arr.updated(i, f(arr(i))))
    else
      None

  trait CreditCard
  trait PaymentProcessor {
    def charge(cc: CreditCard, price: Double): Unit
    def charge2(cc: CreditCard, price: Double): Charge
  }

  case class Coffee(sugar: Int) {
    val price = 2.5
  }

  def buyCoffee1(withSugar: Option[Int], p: PaymentProcessor, cc: CreditCard): Coffee = {
    val cup =
      withSugar.fold(Coffee(0))(n => Coffee(n))
    p.charge(cc, cup.price)
    cup
  }
  final case class Charge(cc: CreditCard, price: Double)
  def buyCoffee2(withSugar: Option[Int], p: PaymentProcessor, cc: CreditCard): (Coffee, Charge) = {
    val cup =
      withSugar.fold(Coffee(0))(n => Coffee(n))
    (cup, p.charge2(cc, cup.price))
  }

  trait Draw {
    def goLeft(): Unit
    def goRight(): Unit
    def goUp(): Unit
    def goDown(): Unit
    def draw(): Unit
    def finish(): List[List[Boolean]]
  }
  def draw1(size: Int): Draw = new Draw {
    val canvas = Array.fill(size, size)(false)
    var x      = 0
    var y      = 0

    def goLeft(): Unit  = x -= 1
    def goRight(): Unit = x += 1
    def goUp(): Unit    = y += 1
    def goDown(): Unit  = y -= 1
    def draw(): Unit = {
      def wrap(x: Int): Int =
        if (x < 0) (size - 1) + ((x + 1) % size) else x % size

      val x2 = wrap(x)
      val y2 = wrap(y)

      canvas.updated(x2, canvas(x2).updated(y2, true))
    }
    def finish(): List[List[Boolean]] =
      canvas.map(_.toList).toList
  }

  sealed trait Command
  object Command {
    case object GoLeft extends Command
    case object GoRight extends Command
    case object GoUp extends Command
    case object GoDown extends Command
    case object Draw extends Command
  }
  def draw2(commands: List[Command], size: Int): List[List[Boolean]] = {
    val canvas = Array.fill(size, size)(false)
    var x      = 0
    var y      = 0

    def parse(c: Command): Unit =
      c match {
        case GoLeft =>
          x -= 1
        case GoRight =>
          x += 1
        case GoUp =>
          y += 1
        case GoDown =>
          y -= 1
        case Draw =>
          def wrap(x: Int): Int =
            if (x < 0) (size - 1) + ((x + 1) % size) else x % size

          val x2 = wrap(x)
          val y2 = wrap(y)

          canvas.updated(x2, canvas(x2).updated(y2, true))
      }

    commands.foreach(parse)
    canvas.map(_.toList).toList
  }

}
