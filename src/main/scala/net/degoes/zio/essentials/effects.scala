// Copyright(C) 2019 - John A. De Goes. All rights reserved.

package net.degoes.zio
package essentials

import net.degoes.zio.essentials.effects.Console._

import scala.util.Try

object effects {

  /**
   * `Console` is an immutable data structure that describes a console program
   * that may involve reading from the console, writing to the console, or
   * returning a value.
   */
  sealed trait Console[A] { self =>
    import Console._

    /**
     * Implement `flatMap` for every type of `Console[A]` to turn it into a
     * `Console[B]` using the function `f`.
     */
    final def flatMap[B](f: A => Console[B]): Console[B] =
      self match {
        case ReadLine(next) =>
          ReadLine(input => next(input).flatMap(f))
        case WriteLine(line, next) =>
          WriteLine(line, next.flatMap(f))
        case Return(thunk) =>
          f(thunk())
      }

    final def map[B](f: A => B): Console[B] = flatMap(f andThen (Console.succeed(_)))

    final def *>[B](that: Console[B]): Console[B] = (self zip that).map(_._2)

    final def <*[B](that: Console[B]): Console[A] = (self zip that).map(_._1)

    /**
     * Implement the `zip` function using `flatMap` and `map`.
     */
    final def zip[B](that: Console[B]): Console[(A, B)] =
      self.flatMap(a => that.map(b => (a, b)))

  }
  object Console {
    final case class ReadLine[A](next: String => Console[A])      extends Console[A]
    final case class WriteLine[A](line: String, next: Console[A]) extends Console[A]
    final case class Return[A](value: () => A)                    extends Console[A]

    /**
     * Implement the following helper functions:
     */

    val readLine: Console[String] =
      ReadLine(input => Return(() => input))

    def writeLine(line: String): Console[Unit] =
      WriteLine(line, Return(() => ()))

    def succeed[A](a: => A): Console[A]        =
      Return(() => a)
  }

  /**
   * Using the helper functions, write a program that just returns a unit value.
   */
  val unit: Console[Unit] =
    succeed(Unit)

  /**
   * Using the helper functions, write a program that just returns the value 42.
   */
  val value: Console[Int] =
    succeed(43)

  /**
   * Using the helper functions, write a program that asks the user for their name.
   */
  val askYourName: Console[Unit] =
    writeLine("What is your name?")

  /**
   * Using the helper functions, write a program that read the name of the user.
   */
  val name: Console[String] =
    readLine

  /***
    * Using `flatMap` and the preceding three functions, write a program that
    * asks the user for their name, reads their name, and greets them.
    */
  val sayHello: Console[Unit] =
    for {
      _     <- askYourName
      name  <- name
      _     <- writeLine(s"Hello $name!")
    } yield ()

  /**
   * Write a program that reads from the console then parse the given input into int if it possible
   * otherwise it returns None
   */
  val readInt: Console[Option[Int]] =
    Console.readLine.map(str => Try(str.toInt).toOption)

  /**
   * implement the following effectful procedure, which interprets
   * the description of a given `Console[A]` into A and run it.
   */
  def unsafeRun[A](program: Console[A]): A =
    program match {
      case Return(thunk) =>
        thunk()
      case WriteLine(line, next) =>
        println(line)
        unsafeRun(next)
      case ReadLine(next) =>
        val line = scala.io.StdIn.readLine()
        unsafeRun(next(line))
    }

  def f1(x: Int)      : Option[Boolean] = ???
  def f2(x: Boolean)  : Option[Int]     = ???
  def f3(x: Int)      : String          = ???

  def foo(x: Int): Option[String] =
    for {
      xf1    <- f1(x)
      xf2    <- f2(xf1) match {
        case Some(0) =>
          None
        case nonZero =>
          nonZero
      }
      xf3     = f3(xf2)
    } yield xf3

  /**
   * implement the following combinator `collectAll` that operates on programs
   */
  def collectAll[A](programs: List[Console[A]]): Console[List[A]] =
    programs.foldRight[Console[List[A]]](succeed(Nil)) {
      case (pA, pList) =>
        pA.zip(pList).map {
          case (head, tail) => head :: tail
        }
    }

  val questions = List("What is your name?",
    "Where were you born?", "Where do you live?",
    "What is your age?", "What is your favorite programming language?")

  val answerProgram: Console[List[String]] =
    collectAll(questions.map(Console.writeLine(_) *> Console.readLine))

  val answerPrograms2: Console[List[String]] = foreach(questions) { question =>
    Console.writeLine(question) *> Console.readLine
  }

  /**
   * implement the `foreach` function that compute a result for each iteration
   */
  def foreach[A, B](values: List[A])(body: A => Console[B]): Console[List[B]] =
    collectAll(values.map(body))

  /**
   * Implement the methods of Thunk
   */
  class Thunk[A](val run: () => A) {
    def map[B](ab: A => B): Thunk[B]             =
      new Thunk(() => ab(run()))
    def flatMap[B](afb: A => Thunk[B]): Thunk[B] =
      new Thunk(() => afb(run()).run())
    def attempt: Thunk[Either[Throwable, A]]     =
      new Thunk(() => Try(run()).toEither)
  }
  object Thunk {
    def succeed[A](a: => A): Thunk[A]   = new Thunk(() => a)
    def fail[A](t: Throwable): Thunk[A] = new Thunk(() => throw t)
  }

  /**
   * Build the version of printLn and readLn
   * then make a simple program base on that.
   */
  def printLn(line: String): Thunk[Unit] = Thunk.succeed(println(line))
  def readLn: Thunk[String]              = Thunk.succeed(scala.io.StdIn.readLine())

  val thunkProgram: Thunk[Unit] =
    for {
      a   <- Thunk.succeed(1)
      b   <- Thunk.succeed(2)
    } yield (a, b)
}
