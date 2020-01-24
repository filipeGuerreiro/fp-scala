package parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

object Par {
    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
        def isDone(): Boolean = true
        def get(timeout: Long, units: TimeUnit) = get
        def isCancelled(): Boolean = false
        def cancel(eventIfRunning: Boolean): Boolean = false
    }

    def fork[A](a: => Par[A]): Par[A] =
        es => es.submit(new Callable[A] {
            def call = a(es).get
        }) 

    def map2[A,B,C](a1: Par[A], a2: Par[B])(f: (A, B) => C): Par[C] =
        (es: ExecutorService) => {
            val af = a1(es)
            val bf = a2(es)
            UnitFuture(f(af.get, bf.get))
        }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

}