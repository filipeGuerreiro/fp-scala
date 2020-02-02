package testing

import state.State
import state.RNG

trait Prop {
    // def check: Either[(FailedCase, SuccessCount), SuccessCount]

    // def &&(p: Prop): Prop = {
    //     Prop(this.check && p.check)
    // }
}

object Prop {
    type SuccessCount = Int
    type FailedCase = String
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
    

    def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
        return null

    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
        Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
    }

}