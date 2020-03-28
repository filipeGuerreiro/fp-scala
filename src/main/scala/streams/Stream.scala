package streams

sealed trait Process[I,O] {
    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs) // Stream is empty
      }
      case Emit(h,t) => h #:: t(s)
    }

    def repeat: Process[I,O] = {
      def go(p: Process[I,O]): Process[I,O] = p match {
        case Halt() => go(this)
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        }
        case Emit(h, t) => Emit(h, go(t))
      }
      go(this)
    }
}

case class Emit[I,O](
    head: O,
    tail: Process[I,O] = Halt[I,O]()) extends Process[I,O]

case class Await[I,O](
    recv: Option[I] => Process[I,O]) extends Process[I,O]

case class Halt[I,O]() extends Process[I,O]

object Process {
    def liftOne[I,O](f: I => O): Process[I,O] = Await {
        case Some(value) => Emit(f(value))
        case None => Halt()
    }

    def lift[I,O](f: I => O): Process[I,O] =
        liftOne(f).repeat


    def await[I,O](f: I => Process[I,O],
                   fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
      Await[I,O] {
        case Some(i) => f(i)
        case None => fallback
      }

    def emit[I,O](head: O,
                  tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
      Emit(head, tail)

    def sum: Process[Double,Double] = {
      def go(acc: Double): Process[Double,Double] =
        await(d => emit(d+acc, go(d+acc)))
      go(0.0)
    }

    def take[I](n: Int): Process[I,I] =
        if (n <= 0) Halt()
        else await(i => emit(i, take(n-1)))

    def drop[I](n: Int): Process[I,I] =
        if (n <= 0) Halt()
        else await(i => drop(n-1))

    def takeWhile[I](f: I => Boolean): Process[I,I] =
        await(i => 
            if (f(i)) emit(i, takeWhile(f))
            else      Halt())

    def dropWhile[I](f: I => Boolean): Process[I,I] =
        await(i => 
            if (f(i)) dropWhile(f)
            else      Halt())
}