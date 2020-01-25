package parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

object Par {
    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

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

    def asyncF[A,B](f: A => B): A => Par[B] =
        a => lazyUnit(f(a))

    def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
        map2(pa, unit())((a,_) => f(a))
    
    def sortPar(pars: Par[List[Int]]) =
        map(pars)(_.sorted)
    
    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
        ps.foldRight[Par[List[A]]](unit(List()))((p, acc) => map2(p, acc)(_ :: _))

    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs =  ps.map(asyncF(f))
        sequence(fbs)
    }
    
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
        val fas = as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
        map(sequence(fas))(_.flatten)
    }
}