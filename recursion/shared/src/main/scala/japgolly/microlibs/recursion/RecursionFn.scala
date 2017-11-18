package japgolly.microlibs.recursion

import scalaz.{Functor, Monad, Traverse, ~>}

object RecursionFn {

  def cata[F[_], A](alg: Algebra[F, A])(implicit F: Functor[F]): Fix[F] => A = {
    var self: Fix[F] => A = null
    self = f => alg(F.map(f.unfix)(self))
    self
  }

  def cataM[M[_], F[_], A](alg: AlgebraM[M, F, A])(implicit M: Monad[M], F: Traverse[F]): Fix[F] => M[A] = {
    var self: Fix[F] => M[A] = null
    self = f => M.bind(F.traverse(f.unfix)(self))(alg)
    self
  }

  def ana[F[_], A](coalg: Coalgebra[F, A])(implicit F: Functor[F]): A => Fix[F] = {
    var self: A => Fix[F] = null
    self = a => Fix[F](F.map(coalg(a))(self))
    self
  }

  def anaM[M[_], F[_], A](coalg: CoalgebraM[M, F, A])(implicit M: Monad[M], F: Traverse[F]): A => M[Fix[F]] = {
    var self: A => M[Fix[F]] = null
    self = a => M.bind(coalg(a))(fa => M.map(F.traverse(fa)(self))(Fix.apply[F]))
    self
  }

  /** ana with immediate cata */
  def hylo[F[_], A, B](coalg: Coalgebra[F, A], alg: Algebra[F, B])(implicit F: Functor[F]): A => B = {
    var self: A => B = null
    self = a => alg(F.map(coalg(a))(self))
    self
  }

  def hyloM[M[_], F[_], A, B](coalg: CoalgebraM[M, F, A], alg: AlgebraM[M, F, B])(implicit M: Monad[M], F: Traverse[F]): A => M[B] = {
    var self: A => M[B] = null
    self = a => M.bind(coalg(a))(fa => M.bind(F.traverse(fa)(self))(alg))
    self
  }

  /** cata that transforms children (outside to inside) before folding */
  def prepro[F[_], A](pro: F ~> F, alg: Algebra[F, A])(implicit F: Functor[F]): Fix[F] => A = {
    val algF : Algebra[F, Fix[F]] = f => Fix[F](pro(f))
    val cataF: Fix[F] => Fix[F]   = cata(algF)
    var self : Fix[F] => A        = null
    val inner: Fix[F] => A        = f => self(cataF(f))
    self                          = f => alg(F.map(f.unfix)(inner))
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
    */
    self
  }

  def postpro[F[_], A](pro: F ~> F, coalg: Coalgebra[F, A])(implicit F: Functor[F]): A => Fix[F] = {
    var self : A => Fix[F]          = null
    val algF : Coalgebra[F, Fix[F]] = f => pro(f.unfix)
    val anaF : Fix[F] => Fix[F]     = ana(algF)
    val inner: A => Fix[F]          = a => anaF(self(a))
    self = a => Fix[F](F.map(coalg(a))(inner))
    self
  }

  /** hylo that can short-circuit on construction */
  def elgot[F[_], A, B](alg: Algebra[F, B], elcoalg: A => B Either F[A])(implicit F: Functor[F]): A => B = {
    var self: A => B = null
    self = a => elcoalg(a) match {
      case Right(fa) => alg(F.map(fa)(self))
      case Left(b)   => b
    }
    self
  }

  /** hylo that can short-circuit on reduction */
  def coelgot[F[_], A, B](coalg: Coalgebra[F, A], elalg: (A, F[B]) => B)(implicit F: Functor[F]): A => B = {
    var self: A => B = null
    self = a => elalg(a, F.map(coalg(a))(self))
    self
  }

}
