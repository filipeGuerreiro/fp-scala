package monoid

trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
}

object Monoid {

    val stringMonoid = new Monoid[String] {
        def op(a1: String, a2: String) = a1 + a2
        def zero = ""
    }

    def listMonoid[A] = new Monoid[List[A]] {
        def op(a1: List[A], a2: List[A]) = a1 ++ a2
        def zero = Nil
    }

    val intAddition = new Monoid[Int] {
        def op(a1: Int, a2: Int): Int = a1 + a2
        def zero: Int = 0
    }

    val intMultiplication = new Monoid[Int] {
        def op(a1: Int, a2: Int): Int = a1 * a2
        def zero: Int = 1
    }

    val booleanOr = new Monoid[Boolean] {
        def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
        def zero: Boolean = false
    }

    val booleanAnd = new Monoid[Boolean] {
        def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
        def zero: Boolean = true
    }

    def optionMonoid[A] = new Monoid[Option[A]] {
        def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
        def zero: Option[A] = None
    }

    def endoMonoid[A] = new Monoid[A => A] {
        def op(a1: A => A, a2: A => A) = a1 compose a2
        def zero = a => a
    }

}