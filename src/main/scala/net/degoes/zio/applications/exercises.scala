// Copyright(C) 2019 - John A. De Goes. All rights reserved.

package net.degoes.zio
package applications

import java.io.IOException

import scalaz.zio._
import scalaz.zio.blocking.Blocking
import scalaz.zio.console.{Console, putStrLn}
import scalaz.zio.duration.Duration

object circuit_breaker extends App {
  import CircuitBreaker.Decider
  import scalaz.zio.clock.{Clock, nanoTime}

  /**
   * implement a circuit breaker mechanism
   */
  class CircuitBreaker[+E](switchClosed: Decider,
                           switchOpen: Long => Boolean,
                           rejectedError: E, ref: Ref[CircuitBreaker.State]) {
    import CircuitBreaker.{Closed, Open}

    def apply[E1 >: E, R, A](zio: ZIO[R, E1, A]): ZIO[R with Clock, E1, A] = {
      def makeOpenEffect(start: Long, end: Long, rejections: Int): (ZIO[R, E1, A], CircuitBreaker.State) = {
        val duration = Duration.fromNanos(end - start)

        if (switchClosed(duration, rejections)) zio -> Closed(0)
        else ZIO.fail(rejectedError) -> Open(start, rejections + 1)
      }

      def makeClosedEffect(start: Long): ZIO[R, E1, A] =
        zio.foldM(
          error => ref.updateSome {
            case Closed(n) =>
              if (switchOpen(n)) Open(start, 0)
              else Closed(n + 1)
          } *> ZIO.fail(error),
          success => ref.updateSome {
            case Closed(_) => Closed(0)
          } *> ZIO.succeed(success)
        )

      for {
        currentTime   <-  nanoTime
        effect        <-  ref.modify[ZIO[R, E1, A]] {
                            case Open(start, rejections)  => makeOpenEffect(start, currentTime, rejections)
                            case state                    => makeClosedEffect(currentTime) -> state
                          }
        result         <- effect
      } yield result
    }

    final def mapError[E1](f: E => E1): CircuitBreaker[E1] = {
      new CircuitBreaker(switchClosed, switchOpen, f(rejectedError), ref)
    }
  }

  object CircuitBreaker {
    sealed trait State
    final case class Closed(runFailures: Int)     extends State
    final case class Open(started: Long, rejections: Int)       extends State

    type Decider = (Duration, Int) => Boolean

    def maxRejections(n: Int): Decider =
      (_, i) => i >= n

    def maxDuration(d: Duration): Decider =
      (d, _) => d >= d

    def both(left: Decider, right: Decider): Decider =
      (d, i) => left(d, i) && right(d, i)

    def either(left: Decider, right: Decider): Decider =
      (d, i) => left(d, i) || right(d, i)

    def make[E](switchClosed: Decider, switchOpen: Long => Boolean, e: E): UIO[CircuitBreaker[E]] =
      Ref.make[CircuitBreaker.State](CircuitBreaker.Closed(0)).map(ref => new CircuitBreaker(switchClosed, switchOpen, e, ref))

  }

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = ???
}

object circuit_breaker1 extends App {
  import CircuitBreaker.Decider
  import scalaz.zio.clock.{Clock, nanoTime}

  /**
    * implement a circuit breaker mechanism
    */
  class CircuitBreaker[+E](switchClosed: Decider,
                           switchOpen: Long => Boolean,
                           rejectedError: E, ref: Ref[CircuitBreaker.State]) {
    import CircuitBreaker.{Closed, Open}

    def apply[E1 >: E, R, A](zio: ZIO[R, E1, A]): ZIO[R with Clock, E1, A] = {
      def makeOpenEffect(start: Long, end: Long, rejections: Int): (ZIO[R, E1, A], CircuitBreaker.State) = {
        val duration = Duration.fromNanos(end - start)

        if (switchClosed(duration, rejections)) zio -> Closed(0)
        else ZIO.fail(rejectedError) -> Open(start, rejections + 1)
      }

      def makeClosedEffect(start: Long): ZIO[R, E1, A] =
        zio.foldM(
          error => ref.updateSome {
            case Closed(n) =>
              if (switchOpen(n)) Open(start, 0)
              else Closed(n + 1)
          } *> ZIO.fail(error),
          success => ref.updateSome {
            case Closed(_) => Closed(0)
          } *> ZIO.succeed(success)
        )

      for {
        currentTime   <-  nanoTime
        effect        <-  ref.modify[ZIO[R, E1, A]] {
          case Open(start, rejections)  => makeOpenEffect(start, currentTime, rejections)
          case state                    => makeClosedEffect(currentTime) -> state
        }
        result         <- effect
      } yield result
    }

    final def mapError[E1](f: E => E1): CircuitBreaker[E1] = {
      new CircuitBreaker(switchClosed, switchOpen, f(rejectedError), ref)
    }
  }

  object CircuitBreaker {
    sealed trait State
    final case class Closed(runFailures: Int)     extends State
    final case class Open(started: Long, rejections: Int)       extends State

    type Decider = (Duration, Int) => Boolean

    def maxRejections(n: Int): Decider =
      (_, i) => i >= n

    def maxDuration(d: Duration): Decider =
      (d, _) => d >= d

    def both(left: Decider, right: Decider): Decider =
      (d, i) => left(d, i) && right(d, i)

    def either(left: Decider, right: Decider): Decider =
      (d, i) => left(d, i) || right(d, i)

    def make[E](switchClosed: Decider, switchOpen: Long => Boolean, e: E): UIO[CircuitBreaker[E]] =
      Ref.make[CircuitBreaker.State](CircuitBreaker.Closed(0)).map(ref => new CircuitBreaker(switchClosed, switchOpen, e, ref))

  }

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = ???
}

object hangman extends App {

  import scalaz.zio.console._
  import scalaz.zio.random._

  final case class State(name: String, guesses: Set[Char], word: String) {
    def failures: Int = (guesses -- word.toSet).size

    def playerLost: Boolean = failures > 10

    def playerWon: Boolean = (word.toSet -- guesses).isEmpty
  }

  def gameLoop(ref: Ref[State]): ZIO[Console, IOException, State] =
    for {
      state     <-  ref.get
      _         <-  renderState(state)
      char      <-  getChoice
      newState   =  state.copy(guesses = state.guesses + char)
      _         <-  ref.set(newState)
      cont      <-  if (newState == state)
                      putStrLn(s"You already guessed the char $char!").const(true)
                    else if (newState.playerLost)
                      putStrLn(s"Sorry, ${newState.name}, you lost the game. Please play again.").const(false)
                    else if (newState.playerWon)
                      putStrLn(s"Congratulations, ${newState.name}, you won the game.").const(false)
                    else if (newState.word.contains(char))
                      putStrLn(s"Great guess, ${newState.name}, keep on going!").const(true)
                    else putStrLn(s"Sorry, ${newState.name}, you did not guess right.").const(true)
      result    <-  if (cont) gameLoop(ref) else ZIO.succeed(state)
    } yield result

  def renderState(state: State): ZIO[Console, Nothing, Unit] = {

    /**
     *
     *  f     n  c  t  o
     *  -  -  -  -  -  -  -
     *
     *  Guesses: a, z, y, x
     *
     */
    val word =
      state.word.toList.map(c => if (state.guesses.contains(c)) s" $c " else "   ").mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    putStrLn(text)
  }

  def getChoice: ZIO[Console, IOException, Char] =
    for {
      _     <-  putStrLn("Please guess a letter:")
      guess <-  getStrLn
      char  <-  guess.trim.toLowerCase.toList match {
                  case char :: Nil => IO.succeed(char)
                  case _           => putStrLn("You did not enter a single letter.") *> getChoice
                }
    } yield char

  def getName: ZIO[Console, IOException, String] =
    for {
      _     <- putStrLn("Please enter your name:")
      name  <- getStrLn
    } yield name

  def chooseWord: ZIO[Random, Nothing, String] =
    nextInt(Dictionary.size).map(int => Dictionary.lift(int).getOrElse("Buggy"))

  val Dictionary = List("aaron", "abelian", "ability", "about", "abstract", "abstract", "abstraction",
    "accurately", "adamek", "add", "adjacent", "adjoint", "adjunction", "adjunctions", "after", "after", "again", "ahrens", "albeit", "algebra", "algebra", "algebraic", "all", "all", "allegories", "almost", "already", "also", "american", "among", "amount", "ams", "an", "an", "analysis", "analytic", "and", "and", "andre", "any", "anyone", "apart", "apologetic", "appears", "applicability", "applications", "applications", "applied", "apply", "applying", "applying", "approach", "archetypical", "archetypical", "are", "areas", "argument", "arising", "aristotle", "arrowsmorphism", "article", "arxiv13026946", "arxiv13030584", "as", "as", "aspect", "assumed", "at", "attempts", "audience", "august", "awodey", "axiom", "axiomatic", "axiomatized", "axioms", "back", "barr", "barry", "basic", "basic", "be", "beginners", "beginning", "behind", "being", "benedikt", "benjamin", "best", "better", "between", "bicategories", "binary", "bodo", "book", "borceux", "both", "both", "bourbaki", "bowdoin", "brash", "brendan", "build", "built", "but", "but", "by", "called", "cambridge", "can", "cardinal", "carlos", "carnap", "case", "cases", "categorial", "categorical", "categorical", "categories", "categories", "categorification", "categorize", "category", "category", "cats", "catsters", "central", "certain", "changes", "charles", "cheng", "chicago", "chiefly", "chopin", "chris", "cite", "clash", "classes", "classical", "closed", "coend", "coin", "colimit", "colin", "collection", "collections", "comparing", "completion", "composed", "composition", "computational", "computer", "computing", "concept", "concepts", "concepts", "conceptual", "concrete", "confronted", "consideration", "considers", "consistently", "construction", "constructions", "content", "contents", "context", "context", "contexts", "continues", "continuous", "contrast", "contributed", "contributions", "cooper", "correctness", "costas", "count", "course", "cover", "covering", "current", "currently", "david", "decategorification", "deducing", "define", "defined", "defining", "definition", "definitions", "der", "derives", "described", "describing", "description", "descriptions", "detailed", "development", "dictum", "did", "different", "dimensions", "directed", "discovered", "discovery", "discuss", "discussed", "discussion", "discussion", "disparage", "disservice", "do", "does", "driving", "drossos", "duality", "dvi", "each", "easy", "ed", "edges", "edit", "edition", "eilenberg", "eilenbergmaclane", "elementary", "elementary", "elements", "elementwise", "elephant", "ellis", "else", "embedding", "embodiment", "embryonic", "emily", "end", "enthusiastic", "equations", "equivalence", "equivalences", "equivalences", "etc", "etcs", "eugenia", "even", "eventually", "everything", "evident", "example", "examples", "examples", "except", "excused", "exist", "exists", "exposure", "expressed", "expressiveness", "extension", "extra", "f", "fact", "fair", "families", "far", "feeds", "feeling", "finds", "finite", "first", "flourished", "focuses", "folklore", "follows", "fong", "for", "for", "force", "forced", "foremost", "form", "formalizes", "formulated", "forthcoming", "found", "foundation", "foundations", "foundations", "francis", "free", "freyd", "freydmitchell", "from", "functions", "functor", "functor", "functors", "fundamental", "further", "gabrielulmer", "general", "general", "generalized", "generalizes", "geometry", "geometry", "george", "geroch", "get", "gift", "give", "given", "going", "goldblatt", "grandis", "graph", "gray", "grothendieck", "ground", "group", "groupoid", "grp", "guide", "göttingen", "had", "handbook", "handful", "handle", "harper", "has", "have", "he", "here", "here", "herrlich", "higher", "higher", "higherdimensional", "highlevel", "hilberts", "his", "historical", "historically", "history", "history", "holistic", "holland", "home", "homomorphisms", "homotopy", "homotopy", "horizontal", "horst", "however", "i", "idea", "ideas", "ieke", "if", "if", "illustrated", "important", "in", "in", "inaccessible", "inadmissible", "include", "includes", "including", "indeed", "indexes", "infinite", "informal", "initial", "innocent", "instance", "instead", "instiki", "interacting", "internal", "intersection", "into", "introduce", "introduced", "introduces", "introducing", "introduction", "introduction", "introductory", "intuitions", "invitation", "is", "isbell", "isbn", "isomorphisms", "it", "it", "its", "itself", "ive", "j", "jaap", "jacob", "jiri", "johnstone", "joy", "jstor", "just", "kan", "kant", "kapulkin", "kashiwara", "kind", "kinds", "kleins", "kmorphisms", "ktransfors", "kℕ", "la", "lagatta", "lane", "language", "large", "last", "later", "later", "latest", "lauda", "lawvere", "lawveres", "lead", "leads", "least", "lectures", "led", "leinster", "lemma", "lemmas", "level", "library", "lifting", "likewise", "limit", "limits", "link", "linked", "links", "list", "literally", "logic", "logic", "logically", "logische", "long", "lurie", "mac", "maclane", "made", "major", "make", "manifest", "many", "many", "mappings", "maps", "marco", "masaki", "material", "mathct0305049", "mathematical", "mathematical", "mathematician", "mathematician", "mathematics", "mathematics", "mathematicsbrit", "may", "mclarty", "mclartythe", "means", "meet", "membership", "methods", "michael", "misleading", "mitchell", "models", "models", "moerdijk", "monad", "monadicity", "monographs", "monoid", "more", "morphisms", "most", "mostly", "motivation", "motivations", "much", "much", "music", "must", "myriads", "named", "natural", "natural", "naturally", "navigation", "ncategory", "necessary", "need", "never", "new", "nlab", "no", "no", "nocturnes", "nonconcrete", "nonsense", "nontechnical", "norman", "north", "northholland", "not", "notes", "notes", "nothing", "notion", "now", "npov", "number", "object", "objects", "obliged", "observation", "observing", "of", "on", "one", "online", "oosten", "operads", "opposed", "or", "order", "originally", "other", "other", "others", "out", "outside", "outside", "over", "packing", "page", "page", "pages", "paper", "paradigm", "pareigis", "parlance", "part", "particularly", "pdf", "pedagogical", "people", "perfect", "perhaps", "perpetrated", "perspective", "peter", "phenomenon", "phil", "philosopher", "philosophers", "philosophical", "philosophy", "physics", "physics", "pierce", "pierre", "played", "pleasure", "pointed", "poset", "possession", "power", "powered", "powerful", "pp", "preface", "prerequisite", "present", "preserving", "presheaf", "presheaves", "press", "prevail", "print", "probability", "problem", "proceedings", "process", "progression", "project", "proof", "property", "provide", "provides", "ps", "publicly", "published", "pure", "purloining", "purpose", "quite", "quiver", "rails", "rather", "reader", "realizations", "reason", "recalled", "record", "references", "reflect", "reflects", "rejected", "related", "related", "relation", "relation", "relations", "representable", "reprints", "reproduce", "resistance", "rests", "results", "reveals", "reverse", "revised", "revisions", "revisions", "rezk", "riehl", "robert", "role", "row", "ruby", "running", "same", "samuel", "saunders", "say", "scedrov", "schanuel", "schapira", "school", "sci", "science", "scientists", "search", "see", "see", "sense", "sep", "sequence", "serious", "set", "set", "sets", "sets", "sheaf", "sheaves", "shortly", "show", "shulman", "similar", "simon", "simple", "simplified", "simply", "simpson", "since", "single", "site", "situations", "sketches", "skip", "small", "so", "society", "some", "some", "sometimes", "sophisticated", "sophistication", "source", "space", "speak", "special", "specific", "specifically", "speculative", "spivak", "sprache", "stage", "standard", "statements", "steenrod", "stephen", "steps", "steve", "still", "stop", "strecker", "structural", "structuralism", "structure", "structures", "students", "study", "studying", "subjects", "such", "suggest", "summer", "supported", "supports", "symposium", "syntax", "tac", "taken", "talk", "tannaka", "tautological", "technique", "tend", "tends", "term", "terminology", "ternary", "tex", "textbook", "textbooks", "texts", "than", "that", "the", "the", "their", "their", "them", "themselves", "then", "theorem", "theorems", "theorems", "theoretic", "theoretical", "theories", "theorist", "theory", "theory", "there", "there", "these", "these", "they", "thinking", "this", "this", "thought", "through", "throughout", "thus", "time", "to", "tom", "tone", "too", "toolset", "top", "topics", "topoi", "topological", "topology", "topologyhomotopy", "topos", "topos", "toposes", "toposes", "transactions", "transformation", "transformations", "trinitarianism", "trinity", "triple", "triples", "trivial", "trivially", "true", "turns", "two", "two", "type", "typically", "uncountable", "under", "under", "understood", "unification", "unify", "unions", "univalent", "universal", "universal", "universes", "university", "use", "used", "useful", "using", "usual", "van", "variants", "various", "vast", "vect", "versatile", "video", "videos", "viewpoint", "views", "vol", "vol", "vs", "was", "way", "we", "wealth", "web", "wells", "were", "what", "when", "when", "where", "which", "while", "whole", "whose", "will", "willerton", "william", "willingness", "with", "witticism", "words", "working", "working", "would", "writes", "xfy", "xfygzxgfz", "xy", "yoneda", "york1964", "youtube")

  /**
   *  Instantiate the polymorphic game to the `IO[Nothing, ?]` type.
   *  by providing `Console`and `Random`
   */
  val myGameIO: ZIO[Console with Random, IOException, Unit] =
    for {
      name  <- getName
      word  <- chooseWord
      ref   <- Ref.make(State(name, Set(), word))
      _     <- gameLoop(ref)
    } yield ()

  /**
   * Create a test data structure that can contain a buffer of lines (to be
   * read from the console), a log of output (that has been written to the
   * console), and a list of "random" numbers.
   */
  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    myGameIO.const(0) orElse ZIO.succeed(1)

  final case class TestScenario(input: List[String], output: List[String], random: Int)

  def testEnvironment(ref: Ref[TestScenario]): Console with Random = new Console with Random {
    val console: Console.Service[Any] = new Console.Service[Any] {
      def putStrLn(line: String): UIO[Unit] =
        ref.modify(state => () -> state.copy(output = line :: state.output))

      val getStrLn: IO[IOException, String] =
        ref.modify(state => state.input.head -> state.copy(input = state.input.drop(1)))

      def putStr(line: String): UIO[Unit] = ???
      def putStr(stream: java.io.PrintStream)(line: String): UIO[Unit] = ???
      def putStrLn(stream: java.io.PrintStream)(line: String): UIO[Unit] = ???
      def getStrLn(reader: java.io.Reader): IO[IOException, String] = ???
    }

    val random: Random.Service[Any] = new Random.Service[Any] {
      val nextBoolean: ZIO[Any,Nothing,Boolean] = UIO(false)
      def nextBytes(length: Int): ZIO[Any,Nothing,scalaz.zio.Chunk[Byte]] = UIO(Chunk.empty)
      val nextDouble: ZIO[Any,Nothing,Double] = UIO(0.0)
      val nextFloat: ZIO[Any,Nothing,Float] = UIO(0.0F)
      val nextGaussian: ZIO[Any,Nothing,Double] = UIO(0.0)
      val nextInt: ZIO[Any,Nothing,Int] = UIO(0)
      def nextInt(n: Int): ZIO[Any,Nothing,Int] = ref.get.map(_.random)
      val nextLong: ZIO[Any,Nothing,Long] = UIO(0L)
      val nextPrintableChar: ZIO[Any,Nothing,Char] = UIO('A')
      def nextString(length: Int): ZIO[Any,Nothing,String] = UIO("foo")
    }
  }

  def testMyGame(scenario: TestScenario): IO[IOException, TestScenario] =
    for {
      ref     <-  Ref.make(scenario)
      env      =  testEnvironment(ref)
      _       <-  myGameIO.provide(env)
      result  <-  ref.get
    } yield result

  val scenario1 =
    TestScenario(
      input  = List("John", "a", "b", "e", "l", "i", "n"),
      output = List(),
      random = 1
    )

  def printTestScenario(scenario: TestScenario): ZIO[Console, IOException, Unit] =
    for {
      scenario  <- testMyGame(scenario)
      _         <- putStrLn("Output from program:")
      _         <- putStrLn(scenario.output.reverse.mkString("\n"))
      _         <- putStrLn("Line of input remaining: " + scenario.input.length)
    } yield ()
}

object parallel_web_crawler {
  import scalaz.zio.clock.Clock
  import scalaz.zio.duration._

  def crawl(
   n         : Int,
   seeds     : Set[URL],
   router    : URL => Set[URL],
   error     : Exception => UIO[Unit],
   processor : (URL, String) => UIO[Unit]): ZIO[Clock with Blocking, Nothing, Unit] = {
    def monitor(waiterM: UIO[Int], empty: Promise[Nothing, Unit], sizeM: UIO[Int]): ZIO[Clock, Nothing, Unit] = {
      for {
        size   <- sizeM
        waiter <- waiterM
        _      <- if (waiter == n && size == 0) empty.succeed(())
        else ZIO.sleep(1.second) *> monitor(waiterM, empty, sizeM)
      } yield ()
    }

    def waiting[A](waiter: Ref[Int], io: UIO[A]): UIO[A] =
      waiter.update(_ + 1).bracket_(waiter.update(_ - 1))(io)

    for {
      waiter  <- Ref.make[Int](0)
      empty   <- Promise.make[Nothing, Unit]
      crawled <- Ref.make[Set[URL]](seeds)
      queue   <- Queue.bounded[URL](10000)
      _       <- queue.offerAll(seeds)

      worker = waiting(waiter, queue.take).flatMap(url =>
        getURL(url).foldM(error, { html =>
          val newURLs = extractURLs(url, html).toSet.flatMap(router)

          processor(url, html) *>
            crawled.modify { crawled =>
              (newURLs -- crawled, crawled ++ newURLs)
            }.flatMap(queue.offerAll(_))
        })
      ).forever
      _       <- ZIO.forkAll(List.fill(n)(worker))
      _       <- monitor(waiter.get, empty, queue.size).fork
      _       <- empty.await
    } yield ()
  }

  final case class URL private (parsed: io.lemonlabs.uri.Url) {
    import io.lemonlabs.uri._

    final def relative(page: String): Option[URL] =
      scala.util
        .Try(parsed.path match {
          case Path(parts) =>
            val whole = parts.dropRight(1) :+ page.dropWhile(_ == '/')

            parsed.withPath(UrlPath(whole))
        })
        .toOption
        .map(new URL(_))

    def url: String = parsed.toString

    override def equals(a: Any): Boolean = a match {
      case that: URL => this.url == that.url
      case _         => false
    }

    override def hashCode: Int = url.hashCode
  }

  object URL {
    import io.lemonlabs.uri._

    def apply(url: String): Option[URL] =
      scala.util.Try(AbsoluteUrl.parse(url)).toOption match {
        case None         => None
        case Some(parsed) => Some(new URL(parsed))
      }
  }

  private val blockingPool = java.util.concurrent.Executors.newCachedThreadPool()

  def getURL(url: URL): ZIO[Blocking, Exception, String] =
    blocking.interruptible(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString).refineOrDie{JustExceptions}

  def extractURLs(root: URL, html: String): List[URL] = {
    val pattern = "href=[\"\']([^\"\']+)[\"\']".r

    scala.util
      .Try({
        val matches = (for (m <- pattern.findAllMatchIn(html)) yield m.group(1)).toList

        for {
          m   <- matches
          url <- URL(m).toList ++ root.relative(m).toList
        } yield url
      })
      .getOrElse(Nil)
  }

  object test {
    val Home          = URL("http://scalaz.org").get
    val Index         = URL("http://scalaz.org/index.html").get
    val ScaladocIndex = URL("http://scalaz.org/scaladoc/index.html").get
    val About         = URL("http://scalaz.org/about").get

    val SiteIndex =
      Map(
        Home          -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        Index         -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        ScaladocIndex -> """<html><body><a href="index.html">Home</a><a href="/about">About</a></body></html>""",
        About         -> """<html><body><a href="home.html">Home</a><a href="http://google.com">Google</a></body></html>"""
      )

    val getURL: URL => IO[Exception, String] =
      (url: URL) =>
        SiteIndex
          .get(url)
          .fold[IO[Exception, String]](IO.fail(new Exception("Could not connect to: " + url)))(IO.effectTotal(_))

    val ScalazRouter: URL => Set[URL] =
      url => if (url.parsed.apexDomain == Some("scalaz.org")) Set(url) else Set()

    val Processor: (URL, String) => IO[Unit, List[(URL, String)]] =
      (url, html) => IO.succeed(List(url -> html))
  }

  def run(args: List[String]): ZIO[Console, Nothing, Int] =
    (for {
      _ <- putStrLn("Hello World!")
    } yield ()).fold(_ => 1, _ => 0)
}
