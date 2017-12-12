package japgolly.microlibs.recursion

import scalaz.{Cofree, Comonad, Free, Functor, Monad, Traverse, ~>}

object RecursionFn {

  def cata[F[_], A](alg: FAlgebra[F, A])(implicit F: Functor[F]): Fix[F] => A = {
    var self: Fix[F] => A = null
    self = f => alg(F.map(f.unfix)(self))
    self
  }

  def cataM[M[_], F[_], A](alg: FAlgebraM[M, F, A])(implicit M: Monad[M], F: Traverse[F]): Fix[F] => M[A] = {
    var self: Fix[F] => M[A] = null
    self = f => M.bind(F.traverse(f.unfix)(self))(alg)
    self
  }

  def ana[F[_], A](coalg: FCoalgebra[F, A])(implicit F: Functor[F]): A => Fix[F] = {
    var self: A => Fix[F] = null
    self = a => Fix[F](F.map(coalg(a))(self))
    self
  }

  def anaM[M[_], F[_], A](coalg: FCoalgebraM[M, F, A])(implicit M: Monad[M], F: Traverse[F]): A => M[Fix[F]] = {
    var self: A => M[Fix[F]] = null
    self = a => M.bind(coalg(a))(fa => M.map(F.traverse(fa)(self))(Fix.apply[F]))
    self
  }

  /** ana with immediate cata */
  def hylo[F[_], A, B](coalg: FCoalgebra[F, A], alg: FAlgebra[F, B])(implicit F: Functor[F]): A => B = {
    var self: A => B = null
    self = a => alg(F.map(coalg(a))(self))
    self
  }

  def hyloM[M[_], F[_], A, B](coalg: FCoalgebraM[M, F, A], alg: FAlgebraM[M, F, B])(implicit M: Monad[M], F: Traverse[F]): A => M[B] = {
    var self: A => M[B] = null
    self = a => M.bind(coalg(a))(fa => M.bind(F.traverse(fa)(self))(alg))
    self
  }

  /** cata that transforms children before folding.
    * Top-most structure (i.e. the input) is not transformed.
    * Outside to inside.
    */
  def prepro[F[_], A](pre: F ~> F, alg: FAlgebra[F, A])(implicit F: Functor[F]): Fix[F] => A = {
    var self : Fix[F] => A        = null
    val algF : FAlgebra[F, Fix[F]] = f => Fix[F](pre(f))
    val cataF: Fix[F] => Fix[F]   = cata(algF)
    val inner: Fix[F] => A        = f => self(cataF(f))
    self                          = f => alg(F.map(f.unfix)(inner))
    /*
    // Inspection
    var space = ""
    self = f => {
      println(s"${space}F  = ${f.toString.replace("ConsF(", "").replace(")", "")}")
      space += "  "
      val step1 = f.unfix
      val step2 = F.map(step1)(inner)
      val step3 = alg(step2)
      space = space.drop(2)
      println(s"${space}FA = $step2")
      println(s"${space} A = $step3")
      step3
    }
    */
    self
  }

  /** ana that creates a structure, transforming each new child (i.e. the entire structure as exists at the end of a pass).
    * Top-most structure (i.e. the end result) is not transformed.
    * Inside to outside.
    */
  def postpro[F[_], A](coalg: FCoalgebra[F, A], pro: F ~> F)(implicit F: Functor[F]): A => Fix[F] = {
    var self : A => Fix[F]          = null
    val algF : FCoalgebra[F, Fix[F]] = f => pro(f.unfix)
    val anaF : Fix[F] => Fix[F]     = ana(algF)
    val inner: A => Fix[F]          = a => anaF(self(a))
    self = a => Fix[F](F.map(coalg(a))(inner))
    /*
    // Inspection
    var space = ""
    self = a => {
      space += "  "
      val step1 = coalg(a)
      val step2 = Fix[F](F.map(coalg(a))(inner))
      space = space.drop(2)
      println(s"${space}/A = $a")
      println(s"${space}FA = $step1")
      println(s"${space}F  = ${step2.toString.replace("ConsF(", "").replace(")", "")}")
      step2
    }
    */
    self
  }

  /** hylo that can short-circuit on construction */
  def elgot[F[_], A, B](elcoalg: A => B Either F[A], alg: FAlgebra[F, B])(implicit F: Functor[F]): A => B = {
    var self: A => B = null
    self = a => elcoalg(a) match {
      case Right(fa) => alg(F.map(fa)(self))
      case Left(b)   => b
    }
    self
  }

  /** hylo that can short-circuit on reduction */
  def coelgot[F[_], A, B](coalg: FCoalgebra[F, A], elalg: (A, () => F[B]) => B)(implicit F: Functor[F]): A => B = {
    var self: A => B = null
    self = a => elalg(a, () => F.map(coalg(a))(self))
    self
  }

  /** cata that has access to current subtree (Fix[F]) as well as that subtree's folded result (A) */
  def para[F[_], A](alg: RAlgebra[F, A])(implicit F: Functor[F]): Fix[F] => A = {
    var self: Fix[F] => A = null
    val fanout: Fix[F] => (Fix[F], A) = x => (x, self(x))
    self = f => alg(F.map(f.unfix)(fanout))
    self
  }

  /** ana that can branch / short-circuit */
  def apo[F[_], A](coalg: RCoalgebra[F, A])(implicit F: Functor[F]): A => Fix[F] = {
    var self: A => Fix[F] = null
    val fanin: Either[Fix[F], A] => Fix[F] = {
      case Left(f) => f
      case Right(a) => self(a)
    }
    self = a => Fix[F](F.map(coalg(a))(fanin))
    self
  }

  /** cata that retains values of all previous (i.e. child) steps */
  def histo[F[_], A](alg: CVAlgebra[F, A])(implicit F: Functor[F]): Fix[F] => A = {
    var self: Fix[F] => A               = null
    var step: Fix[F] => Cofree[F, A]    = null
    val x   : Fix[F] => F[Cofree[F, A]] = f => F.map(f.unfix)(step)
    self                                = f => alg(x(f))
    step                                = f => Cofree(self(f), x(f))
    // TODO Add variant?
    // val m = collection.mutable.HashMap.empty[Fix[F], Cofree[F, A]]
    // step = f => m.getOrElseUpdate(f, Cofree(self(f), x(f)))
    self
  }

  /** ana that can build multiple levels in a single pass */
  def futu[F[_], A](coalg: CVCoalgebra[F, A])(implicit F: Functor[F]): A => Fix[F] = {
    var self: A => Fix[F] = null
    var step: Free[F, A] => Fix[F] = null
    self = a => Fix[F](F.map(coalg(a))(step))
    step = _.fold(self, f => Fix(F.map(f)(step)))
    self
  }

  /** hylo of futu into histo */
  def chrono[F[_], A, B](coalg: CVCoalgebra[F, A], alg: CVAlgebra[F, B])(implicit F: Functor[F]): A => B =
    // histo(alg)(futu(coalg)(a)) // Naive
    ghylo[Cofree[F, ?], F, Free[F, ?], A, B](distHisto[F], distFutu[F], alg, coalg)

  private type Coseq[F[_], G[_]] = Lambda[A => F[G[A]]] ~> Lambda[A => G[F[A]]]

  private def ghylo[W[_], F[_], M[_], A, B](w: Coseq[F, W],
                                            m: Coseq[M, F],
                                            f: F[W[B]] => B,
                                            g: A => F[M[A]]
                                           )(implicit
                                             W: Comonad[W],
                                             F: Functor[F],
                                             M: Monad[M]): A => B = {
    val liftG: M[A] => M[F[M[A]]] = M.lift(g)
    var h: M[A] => W[B] = null
    h = ma => {
      val fmma: F[M[M[A]]] = m(liftG(ma))
      val fwwb: F[W[W[B]]] = F.map(fmma)(mma => W.cojoin(h(M.join(mma))))
      W.map(w(fwwb))(f)
    }
    a => W.copoint(h(M.point(a)))
  }

  private def distHisto[F[_]](implicit F: Functor[F]): Coseq[F, Cofree[F, ?]] =
    new Coseq[F, Cofree[F, ?]] {
      override def apply[A](f: F[Cofree[F, A]]): Cofree[F, F[A]] =
        Cofree.unfold[F, F[A], F[Cofree[F, A]]](f)(as =>
          (F.map(as)(_.head), F.map(as)(_.tail)))
    }

  private def distFutu[F[_]](implicit F: Functor[F]): Coseq[Free[F, ?], F] =
    new Coseq[Free[F, ?], F] {
      override def apply[A](f: Free[F, F[A]]): F[Free[F, A]] =
        f.fold(
          F.map(_)(Free.pure),
          F.map(_)(as => Free(apply(as))))
    }

}
