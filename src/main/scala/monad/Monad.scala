package monad

import testing.Gen
import parallelism.Par.Par
import parallelism.Par
import state.State

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))
  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(value)  => map(value)(Left(_))
    case Right(value) => map(value)(Right(_))
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]()))((fa, fas) => map2(fa, fas)((a, l) => a :: l))
    
  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, lbs) => map2(f(a), lbs)((b, bs) => b :: bs))
    
  def replicateN[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A,B](ma: F[A], mb: F[B]): F[(A,B)] =
    map2(ma, mb)((a,b) => (a,b))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]()))((a, acc) => 
      flatMap(f(a))(b => 
        if (b) map2(unit(a), acc)((x, xs) => x :: xs)
        else acc))

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(b => g(b))

  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)
}

object Monad {

  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)
  }

  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(fa)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Option(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] =
      fa.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    def unit[A](a: => A): State[S,A] = State.unit(a)
    def flatMap[A, B](fa: State[S,A])(f: A => State[S,B]): State[S,B] =
      fa.flatMap(f)
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R,A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(r => a)
    def flatMap[A, B](fa: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      Reader(r => f(fa.run(r)).run(r))
  }
}
