package japgolly.microlibs.recursion

import scalaz.{Functor, Monad, Traverse, ~>}

object Recursion {
  def cata[F[_], A](alg: Algebra[F, A])(f: Fix[F])(implicit F: Functor[F]): A =
    RecursionFn.cata(alg).apply(f)

  def cataM[M[_], F[_], A](alg: AlgebraM[M, F, A])(f: Fix[F])(implicit M: Monad[M], F: Traverse[F]): M[A] = {
    var self: Fix[F] => M[A] = null
    self = f => M.bind(F.traverse(f.unfix)(self))(alg)
    self(f)
  }

  def ana[F[_], A](coalg: Coalgebra[F, A])(a: A)(implicit F: Functor[F]): Fix[F] = {
    var self: A => Fix[F] = null
    self = a => Fix[F](F.map(coalg(a))(self))
    self(a)
  }

  def anaM[M[_], F[_], A](coalg: CoalgebraM[M, F, A])(a: A)(implicit M: Monad[M], F: Traverse[F]): M[Fix[F]] = {
    var self: A => M[Fix[F]] = null
    self = a => M.bind(coalg(a))(fa => M.map(F.traverse(fa)(self))(Fix.apply[F]))
    self(a)
  }

  def hylo[F[_], A, B](coalg: Coalgebra[F, A], alg: Algebra[F, B])(a: A)(implicit F: Functor[F]): B = {
    var self: A => B = null
    self = a => alg(F.map(coalg(a))(self))
    self(a)
  }

  def hyloM[M[_], F[_], A, B](coalg: CoalgebraM[M, F, A], alg: AlgebraM[M, F, B])(a: A)(implicit M: Monad[M], F: Traverse[F]): M[B] = {
    var self: A => M[B] = null
    self = a => M.bind(coalg(a))(fa => M.bind(F.traverse(fa)(self))(alg))
    self(a)
  }

  /** cata that transforms children (outside to inside) before folding */
  def prepro[F[_], A](pro: F ~> F, alg: Algebra[F, A])(f: Fix[F])(implicit F: Functor[F]): A = {
    val algF : Algebra[F, Fix[F]] = f => Fix[F](pro(f))
    val cataF: Fix[F] => Fix[F]   = RecursionFn.cata(algF)
    var self : Fix[F] => A        = null
    val inner: Fix[F] => A        = f => self(cataF(f))
    self                          = f => alg(F.map(f.unfix)(inner))
    self(f)
    /*
    // Inspection
    var space = ""
    var self: Fix[F] => A = null
    self = f => {
      println(s"${space}F  = ${f.toString.replace("ConsF(", "").replace(")", "")}")
      space += "  "
      val step1 = f.unfix
      val step2 = F.map(step1)(ff => self(cata[F, Fix[F]](q => Fix(pro(q)))(ff)))
      val step3 = alg(step2)
      space = space.drop(2)
      println(s"${space}FA = $step2")
      println(s"${space} A = $step3")
      step3
    }
    self(f)
    */
  }

  def postpro[F[_], A](pro: F ~> F, coalg: Coalgebra[F, A])(a: A)(implicit F: Functor[F]): Fix[F] = {
    var self: A => Fix[F] = null
    val algF: Coalgebra[F, Fix[F]] = f => pro(f.unfix)
    val inner: A => Fix[F] = a => ana(algF)(self(a))
    self = a => Fix[F](F.map(coalg(a))(inner))
    self(a)
  }

  /** hylo that can short-circuit on construction */
  def elgot[F[_], A, B](alg: Algebra[F, B], elcoalg: A => B Either F[A])(a: A)(implicit F: Functor[F]): B = {
    var self: A => B = null
    self = a => elcoalg(a) match {
      case Right(fa) => alg(F.map(fa)(self))
      case Left(b) => b
    }
    self(a)
  }

  /** hylo that can short-circuit on reduction */
  def coelgot[F[_], A, B](coalg: Coalgebra[F, A], elalg: (A, F[B]) => B)(a: A)(implicit F: Functor[F]): B = {
    var self: A => B = null
    self = a => elalg(a, F.map(coalg(a))(self))
    self(a)
  }

}
