package testing

import state.State
import state.RNG
import Gen._
import Prop._

case class Prop(run: (TestCases,RNG) => Result) {
    // def check: Either[(FailedCase, SuccessCount), SuccessCount]

    def &&(p: Prop) = Prop {
        (cases, rng) => run(cases, rng) match {
            case Passed => p.run(cases, rng)
            case x => x
        }
    }

    def ||(p: Prop) = Prop {
        (cases, rng) => run(cases, rng) match {
            case Falsified(_,_) => p.run(cases, rng)
            case x => x
        }
    }
}

object Prop {
    type TestCases = Int
    type SuccessCount = Int
    type FailedCase = String

    sealed trait Result {
        def isFalsified: Boolean
    }

    case object Passed extends Result {
        def isFalsified: Boolean = false
    }

    case class Falsified(failure: FailedCase,
                         successes: SuccessCount) extends Result {
        def isFalsified: Boolean = true
    }
}

case class Gen[+A](sample: State[RNG,A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen(sample.flatMap(a => f(a).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
        size.flatMap(n => Gen.listOfN(n, this))
}

object Gen {

    def unit[A](a: => A): Gen[A] =
        Gen(State.unit(a))

    def boolean: Gen[Boolean] =
        Gen(State(RNG.boolean))

    def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
        Gen(State.sequence(List.fill(n)(a.sample)))
    
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
        boolean.flatMap(b => if (b) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
        val g1Prob = g1._2 / (g1._2 + g2._2)
        Gen(State(RNG.double).flatMap(rng => if (rng > g1Prob) g1._1.sample else g2._1.sample))
    }

    def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
        (n,rng) => randomStream(a)(rng).zip(Stream.from(0)).take(n).map {
            case (a, i) => try {
                if (f(a)) Passed else Falsified(a.toString, i)
            } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }.find(_.isFalsified).getOrElse(Passed)
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
        Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
        s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"        

    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
        Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
    }

}