package stream

object StreamTransducer {

  sealed trait Process[I, O] {
    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) =>
        s match {
          case h #:: t => recv(Some(h))(t)
          case xs      => recv(None)(xs) // Stream is empty
        }
      case Emit(h, t) => h #:: t(s)
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() => go(this)
        case Await(recv) =>
          Await {
            case None => recv(None)
            case i    => go(recv(i))
          }
        case Emit(h, t) => Emit(h, go(t))
      }
      go(this)
    }

    def |>[O2](p2: Process[O, O2]): Process[I, O2] =
      p2 match {
        case Halt()     => Halt()
        case Emit(h, t) => Emit(h, this |> t)
        case Await(f) =>
          this match {
            case Halt()     => Halt()
            case Emit(h, t) => t |> f(Some(h))
            case Await(g)   => Await((i: Option[I]) => g(i) |> p2)
          }
      }
  }

  case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]())
      extends Process[I, O]

  case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

  case class Halt[I, O]() extends Process[I, O]

  object Process {
    def liftOne[I, O](f: I => O): Process[I, O] = Await {
      case Some(value) => Emit(f(value))
      case None        => Halt()
    }

    def lift[I, O](f: I => O): Process[I, O] =
      liftOne(f).repeat

    def await[I, O](
        f: I => Process[I, O],
        fallback: Process[I, O] = Halt[I, O]()
    ): Process[I, O] =
      Await[I, O] {
        case Some(i) => f(i)
        case None    => fallback
      }

    def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
      Emit(head, tail)

    def sum: Process[Double, Double] = {
      def go(acc: Double): Process[Double, Double] =
        await(d => emit(d + acc, go(d + acc)))
      go(0.0)
    }

    def take[I](n: Int): Process[I, I] =
      if (n <= 0) Halt()
      else await(i => emit(i, take(n - 1)))

    def drop[I](n: Int): Process[I, I] =
      if (n <= 0) Halt()
      else await(i => drop(n - 1))

    def takeWhile[I](f: I => Boolean): Process[I, I] =
      await(i =>
        if (f(i)) emit(i, takeWhile(f))
        else Halt()
      )

    def dropWhile[I](f: I => Boolean): Process[I, I] =
      await(i =>
        if (f(i)) dropWhile(f)
        else Halt()
      )

    def count[I]: Process[I, Int] = {
      def go(n: Int): Process[I, Int] =
        await(_ => emit(n + 1, go(n + 1)))
      go(0)
    }

    def mean: Process[Double, Double] = {
      def go(sum: Double, count: Int): Process[Double, Double] =
        await((d: Double) =>
          emit((sum + d) / (count + 1), go(sum + d, count + 1))
        )
      go(0.0, 0)
    }

    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
      await((i: I) =>
        f(i, z) match {
          case (o, s2) => emit(o, loop(s2)(f))
        }
      )

    def sumV2: Process[Double, Double] =
      loop(0.0)((i, acc) => (acc + i, acc + i))
  }
}

object GeneralizedStreamTransducer {
  trait Process[F[_], O] {
    def onHalt(f: Throwable => Process[F,O]): Process[F,O] = this match {
      case Halt(e) => Try(f(e))
      case Emit(h, t) => Emit(h, t.onHalt(f))
      case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
    }

    def ++(p: => Process[F,O]): Process[F,O] =
      this.onHalt {
        case End => Try(p)
        case err => Halt(err)
      }
  }

  case class Await[F[_], A, O](
    req: F[A], 
    recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]
  case class Halt[F[_], O](err: Throwable) extends Process[F, O]
  case class Emit[F[_], O](head: O, tail: Process[F, O])
      extends Process[F, O]

  case object End extends Exception
  case object Kill extends Exception

  def Try[F[_],O](p: => Process[F,O]): Process[F,O] =
      try p
      catch { case e: Throwable => Halt(e) }
}
