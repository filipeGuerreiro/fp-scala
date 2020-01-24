package parallelism

object Par {

    def unit[A](a: A): Par[A]

    def get[A](a: Par[A]): A

    def map2[A,B,C](a1: Par[A], a2: Par[B])(f: (A, B) => C): Par[C]

}