package io

import monad.Monad
import scala.annotation.tailrec

sealed trait Free[F[_],A] {
    def flatMap[B](f: A => Free[F,B]): Free[F,B] =
        FlatMap(this, f)
    def map[B](f: A => B): Free[F,B] =
        flatMap(f andThen (Return(_)))
}
case class Return [F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](s: Free[F,A],
                             f: A => Free[F,B]) extends Free[F,B]

object Free {
    def freeMonad[F[_]] = new Monad[({type f[a] = Free[F,a]})#f] {
        override def unit[A](a: => A): Free[F,A] = Return(a)
        override def flatMap[A, B](fa: Free[F,A])(f: A => Free[F,B]): Free[F,B] = fa.flatMap(f)
    }

    @tailrec
    def runTrampoline[A](a: Free[Function0,A]): A = a match {
        case Return(a) => a
        case Suspend(s) => s()
        case FlatMap(s, f) => s match {
            case Return(a) => runTrampoline(f(a))
            case Suspend(s) => runTrampoline(f(s()))
            case FlatMap(s0, g) => runTrampoline(s0.flatMap(s0 => g(s0).flatMap(f)))
        }
    }
}