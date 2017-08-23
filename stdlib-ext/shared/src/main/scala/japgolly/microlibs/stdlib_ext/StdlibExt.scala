package japgolly.microlibs.stdlib_ext

import java.io.{PrintWriter, StringWriter}
import java.time.Duration
import scala.reflect.ClassTag

object StdlibExt {

  // JSLE prefix = Japgolly StdLib Ext

  implicit class JSLE_String(private val s: String) extends AnyVal {
    def indent(i: String): String =
      if (i.isEmpty)
        s
      else
        i + s.replace("\n", "\n" + i)

    def indent(spaces: Int): String =
      if (spaces <= 0)
        s
      else
        indent(" " * spaces)

    def removeAnsiEscapeCodes: String =
      s.replaceAll("[\u001b\u009b][\\[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-ORZcf-nqry=><]", "")
  }

  implicit class JSLE_Some[A](private val s: Some[A]) extends AnyVal {
    @inline def asOption: Option[A] = s.asInstanceOf[Option[A]]
  }

  implicit class JSLE_OptionObj(private val self: Option.type) extends AnyVal {
    def when[A](cond: Boolean)(a: => A): Option[A] =
       if (cond) Some(a) else None

    @inline def unless[A](cond: Boolean)(a: => A): Option[A] =
       when(!cond)(a)
  }

  implicit class JSLE_Option[A](private val o: Option[A]) extends AnyVal {
    def toMapEntrySetFn[K]: (Map[K, A], K) => Map[K, A] =
      o match {
        case Some(a) => _.updated(_, a)
        case None    => _ - _
      }
  }

  implicit class JSLE_OptionMap[K, V](private val o: Option[Map[K, V]]) extends AnyVal {
    /** Convenience method to avoid Scala's shitty type inference */
    def orEmptyMap: Map[K, V] =
      o getOrElse Map.empty
  }

  implicit class JSLE_Iterator[A](private val as: Iterator[A]) extends AnyVal {

    def filterSubType[T <: A](implicit t: ClassTag[T]): Iterator[T] =
      as.flatMap(t.unapply(_).iterator)

    def nextOption(): Option[A] =
      if (as.hasNext)
        Some(as.next())
      else
        None

    def drain(): Unit =
      while (as.hasNext)
        as.next()

    def intersperse(insert: A): Iterator[A] =
      if (as.isEmpty)
        as
      else {
        val h = as.next()
        Iterator.single(h) ++ as.flatMap(insert :: _ :: Nil)
      }
  }

  implicit class JSLE_IteratorOption[A](private val as: Iterator[Option[A]]) extends AnyVal {
    def nextOptionO: Option[A] =
      if (as.hasNext)
        as.next()
      else
        None

    def firstDefined: Option[A] =
      as.filter(_.isDefined).nextOptionO

    def filterDefined: Iterator[A] =
      as.filter(_.isDefined).map(_.get)
  }

  implicit class JSLE_Vector[A](private val as: Vector[A]) extends AnyVal {
    def isIndexValid(i: Int): Boolean =
      i < as.length && i >= 0

    def get(index: Int): Option[A] =
      getFlatMap(index)(Some(_))

    def getOrElse(index: Int, default: => A): A =
      if (isIndexValid(index))
        as(index)
      else
        default

    def getFlatMap[B](index: Int)(f: A => Option[B]): Option[B] =
      if (isIndexValid(index))
        f(as(index))
      else
        None

    def updateIndexOrNull[B >: A](index: Int, f: A => B): Vector[B] =
      if (isIndexValid(index))
        as.updated(index, f(as(index)))
      else
        null

    def updateIndex[B >: A](index: Int, f: A => B): Option[Vector[B]] =
      Option(updateIndexOrNull(index, f))

    def tryUpdateIndexOrNull[B >: A](index: Int, f: A => Option[B]): Vector[B] =
      if (isIndexValid(index))
        f(as(index)) match {
          case None    => null
          case Some(b) => as.updated(index, b)
        }
      else
        null

    def tryUpdateIndex[B >: A](index: Int, f: A => Option[B]): Option[Vector[B]] =
      Option(tryUpdateIndexOrNull(index, f))

    def insertBefore[B >: A](index: Int, b: B): Option[Vector[B]] =
      if (index == 0)
        Some(b +: as)
      else if (index < 0)
        None
      else (index - as.length) match {
        case 0          => Some(as :+ b)
        case n if n < 0 => Some(as.patch(index, b :: Nil, 0))
        case _          => None
      }

    def deleteOrNull(index: Int): Vector[A] =
      if (isIndexValid(index))
        as.patch(index, Nil, 1)
      else
        null

    def delete(index: Int): Option[Vector[A]] =
      Option(deleteOrNull(index))
  }

  implicit class JSLE_VectorNullable[A >: Null](private val as: Vector[A]) extends AnyVal {
    def getOrNull(index: Int): A =
      if (as.isIndexValid(index))
        as(index)
      else
        null
  }

  implicit class JSLE_Map[K, V](private val m: Map[K, V]) extends AnyVal {

    def setValueOption(k: K, v: Option[V]): Map[K, V] =
      v.fold(m - k)(m.updated(k, _))

    def modifyValueOption(k: K, f: Option[V] => Option[V]): Map[K, V] = {
      val before = m.get(k)
      val after = f(before)
      (before, after) match {
        case (_, Some(v)) => m.updated(k, v)
        case (Some(_), None) => m - k
        case (None, None) => m
      }
    }

    def modifyValueFromOption(k: K, f: Option[V] => V): Map[K, V] =
      m.updated(k, f(m.get(k)))

    def initAndModifyValue(k: K, init: => V, f: V => V): Map[K, V] =
      m.updated(k, f(m.getOrElse(k, init)))

    def setOrModifyValue(k: K, set: => V, modify: V => V): Map[K, V] =
      modifyValueFromOption(k, _.fold(set)(modify))

    def mapOrRemoveEntries[X, Y](f: (K, V) => Option[(X, Y)]): Map[X, Y] = {
      val b = Map.newBuilder[X, Y]
      for {t <- m; xy <- f(t._1, t._2)}
        b += xy
      b.result()
    }

    def mapOrRemoveKeys[X](f: K => Option[X]): Map[X, V] = {
      val b = Map.newBuilder[X, V]
      for {t <- m; x <- f(t._1)}
        b.+=((x, t._2))
      b.result()
    }

    def mapOrRemoveValues[X](f: V => Option[X]): Map[K, X] = {
      val b = Map.newBuilder[K, X]
      for {t <- m; x <- f(t._2)}
        b.+=((t._1, x))
      b.result()
    }

    def mapEntriesNow[X, Y](f: (K, V) => (X, Y)): Map[X, Y] = {
      val b = Map.newBuilder[X, Y]
      for (t <- m)
        b += f(t._1, t._2)
      b.result()
    }

    def mapKeysNow[X](f: K => X): Map[X, V] =
      mapEntriesNow((k, v) => (f(k), v))

    def mapValuesNow[X](f: V => X): Map[K, X] =
      mapEntriesNow((k, v) => (k, f(v)))
  }

  implicit class JSLE_Throwable(private val t: Throwable) extends AnyVal {
    def stackTraceAsString: String = {
      val sw = new StringWriter()
      try {
        val pw = new PrintWriter(sw)
        try {
          t.printStackTrace(pw)
          sw.toString
        } finally pw.close()
      } finally sw.close()
    }

    def stackTraceAsStringWithLineMod(pf: PartialFunction[String, String]): String =
      stackTraceAsStringWithLineExpansion(pf.andThen(_ :: Nil))

    def stackTraceAsStringWithLineExpansion(pf: PartialFunction[String, List[String]]): String =
      stackTraceAsString
        .split('\n')
        .toIterator
        .flatMap(l => pf.lift(l).getOrElse(l :: Nil))
        .mkString("\n")
  }

  @inline implicit class JSLE_Long(private val n: Long) extends AnyVal {
    @inline def nanos  : Duration = Duration.ofNanos(n)
    @inline def micros : Duration = (n * 1000).nanos
    @inline def millis : Duration = Duration.ofMillis(n)
    @inline def seconds: Duration = Duration.ofSeconds(n)
    @inline def minutes: Duration = Duration.ofMinutes(n)
    @inline def hours  : Duration = Duration.ofHours(n)
    @inline def days   : Duration = Duration.ofDays(n)
  }

  @inline implicit def JSLE_IntLong(n: Int): JSLE_Long = new JSLE_Long(n)

  @inline implicit class JSLE_Double(private val n: Double) extends AnyVal {
    def nanos  : Duration = n.toLong.nanos
    def micros : Duration = (n * 1000).nanos
    def millis : Duration = (n * 1000).micros
    def seconds: Duration = (n * 1000).millis
    def minutes: Duration = (n * 60).seconds
    def hours  : Duration = (n * 60).minutes
    def days   : Duration = (n * 24).hours
  }

  // Generated by bin/gen-function_ext

  @inline final implicit class JSLE_Function1ToBool[A](private val x: (A) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A) ⇒ Boolean = !x(_)
    @inline def &&(y: (A)⇒Boolean): (A)⇒Boolean = (a) ⇒ x(a) && y(a)
    @inline def ||(y: (A)⇒Boolean): (A)⇒Boolean = (a) ⇒ x(a) || y(a)
  }
  @inline final implicit class JSLE_Function2ToBool[A,B](private val x: (A,B) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B) ⇒ Boolean = !x(_,_)
    @inline def &&(y: (A,B)⇒Boolean): (A,B)⇒Boolean = (a,b) ⇒ x(a,b) && y(a,b)
    @inline def ||(y: (A,B)⇒Boolean): (A,B)⇒Boolean = (a,b) ⇒ x(a,b) || y(a,b)
  }
  @inline final implicit class JSLE_Function3ToBool[A,B,C](private val x: (A,B,C) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C) ⇒ Boolean = !x(_,_,_)
    @inline def &&(y: (A,B,C)⇒Boolean): (A,B,C)⇒Boolean = (a,b,c) ⇒ x(a,b,c) && y(a,b,c)
    @inline def ||(y: (A,B,C)⇒Boolean): (A,B,C)⇒Boolean = (a,b,c) ⇒ x(a,b,c) || y(a,b,c)
  }
  @inline final implicit class JSLE_Function4ToBool[A,B,C,D](private val x: (A,B,C,D) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D) ⇒ Boolean = !x(_,_,_,_)
    @inline def &&(y: (A,B,C,D)⇒Boolean): (A,B,C,D)⇒Boolean = (a,b,c,d) ⇒ x(a,b,c,d) && y(a,b,c,d)
    @inline def ||(y: (A,B,C,D)⇒Boolean): (A,B,C,D)⇒Boolean = (a,b,c,d) ⇒ x(a,b,c,d) || y(a,b,c,d)
  }
  @inline final implicit class JSLE_Function5ToBool[A,B,C,D,E](private val x: (A,B,C,D,E) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E) ⇒ Boolean = !x(_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E)⇒Boolean): (A,B,C,D,E)⇒Boolean = (a,b,c,d,e) ⇒ x(a,b,c,d,e) && y(a,b,c,d,e)
    @inline def ||(y: (A,B,C,D,E)⇒Boolean): (A,B,C,D,E)⇒Boolean = (a,b,c,d,e) ⇒ x(a,b,c,d,e) || y(a,b,c,d,e)
  }
  @inline final implicit class JSLE_Function6ToBool[A,B,C,D,E,F](private val x: (A,B,C,D,E,F) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F) ⇒ Boolean = !x(_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F)⇒Boolean): (A,B,C,D,E,F)⇒Boolean = (a,b,c,d,e,f) ⇒ x(a,b,c,d,e,f) && y(a,b,c,d,e,f)
    @inline def ||(y: (A,B,C,D,E,F)⇒Boolean): (A,B,C,D,E,F)⇒Boolean = (a,b,c,d,e,f) ⇒ x(a,b,c,d,e,f) || y(a,b,c,d,e,f)
  }
  @inline final implicit class JSLE_Function7ToBool[A,B,C,D,E,F,G](private val x: (A,B,C,D,E,F,G) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G) ⇒ Boolean = !x(_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G)⇒Boolean): (A,B,C,D,E,F,G)⇒Boolean = (a,b,c,d,e,f,g) ⇒ x(a,b,c,d,e,f,g) && y(a,b,c,d,e,f,g)
    @inline def ||(y: (A,B,C,D,E,F,G)⇒Boolean): (A,B,C,D,E,F,G)⇒Boolean = (a,b,c,d,e,f,g) ⇒ x(a,b,c,d,e,f,g) || y(a,b,c,d,e,f,g)
  }
  @inline final implicit class JSLE_Function8ToBool[A,B,C,D,E,F,G,H](private val x: (A,B,C,D,E,F,G,H) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H) ⇒ Boolean = !x(_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H)⇒Boolean): (A,B,C,D,E,F,G,H)⇒Boolean = (a,b,c,d,e,f,g,h) ⇒ x(a,b,c,d,e,f,g,h) && y(a,b,c,d,e,f,g,h)
    @inline def ||(y: (A,B,C,D,E,F,G,H)⇒Boolean): (A,B,C,D,E,F,G,H)⇒Boolean = (a,b,c,d,e,f,g,h) ⇒ x(a,b,c,d,e,f,g,h) || y(a,b,c,d,e,f,g,h)
  }
  @inline final implicit class JSLE_Function9ToBool[A,B,C,D,E,F,G,H,I](private val x: (A,B,C,D,E,F,G,H,I) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I)⇒Boolean): (A,B,C,D,E,F,G,H,I)⇒Boolean = (a,b,c,d,e,f,g,h,i) ⇒ x(a,b,c,d,e,f,g,h,i) && y(a,b,c,d,e,f,g,h,i)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I)⇒Boolean): (A,B,C,D,E,F,G,H,I)⇒Boolean = (a,b,c,d,e,f,g,h,i) ⇒ x(a,b,c,d,e,f,g,h,i) || y(a,b,c,d,e,f,g,h,i)
  }
  @inline final implicit class JSLE_Function10ToBool[A,B,C,D,E,F,G,H,I,J](private val x: (A,B,C,D,E,F,G,H,I,J) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J)⇒Boolean): (A,B,C,D,E,F,G,H,I,J)⇒Boolean = (a,b,c,d,e,f,g,h,i,j) ⇒ x(a,b,c,d,e,f,g,h,i,j) && y(a,b,c,d,e,f,g,h,i,j)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J)⇒Boolean): (A,B,C,D,E,F,G,H,I,J)⇒Boolean = (a,b,c,d,e,f,g,h,i,j) ⇒ x(a,b,c,d,e,f,g,h,i,j) || y(a,b,c,d,e,f,g,h,i,j)
  }
  @inline final implicit class JSLE_Function11ToBool[A,B,C,D,E,F,G,H,I,J,K](private val x: (A,B,C,D,E,F,G,H,I,J,K) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J,K) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J,K)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k) ⇒ x(a,b,c,d,e,f,g,h,i,j,k) && y(a,b,c,d,e,f,g,h,i,j,k)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J,K)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k) ⇒ x(a,b,c,d,e,f,g,h,i,j,k) || y(a,b,c,d,e,f,g,h,i,j,k)
  }
  @inline final implicit class JSLE_Function12ToBool[A,B,C,D,E,F,G,H,I,J,K,L](private val x: (A,B,C,D,E,F,G,H,I,J,K,L) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J,K,L) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J,K,L)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l) && y(a,b,c,d,e,f,g,h,i,j,k,l)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J,K,L)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l) || y(a,b,c,d,e,f,g,h,i,j,k,l)
  }
  @inline final implicit class JSLE_Function13ToBool[A,B,C,D,E,F,G,H,I,J,K,L,M](private val x: (A,B,C,D,E,F,G,H,I,J,K,L,M) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J,K,L,M) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J,K,L,M)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m) && y(a,b,c,d,e,f,g,h,i,j,k,l,m)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J,K,L,M)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m) || y(a,b,c,d,e,f,g,h,i,j,k,l,m)
  }
  @inline final implicit class JSLE_Function14ToBool[A,B,C,D,E,F,G,H,I,J,K,L,M,N](private val x: (A,B,C,D,E,F,G,H,I,J,K,L,M,N) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J,K,L,M,N) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n) && y(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n) || y(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
  }
  @inline final implicit class JSLE_Function15ToBool[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](private val x: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) && y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) || y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
  }
  @inline final implicit class JSLE_Function16ToBool[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](private val x: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) && y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) || y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
  }
  @inline final implicit class JSLE_Function17ToBool[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](private val x: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) && y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) || y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
  }
  @inline final implicit class JSLE_Function18ToBool[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](private val x: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) && y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) || y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
  }
  @inline final implicit class JSLE_Function19ToBool[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](private val x: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) && y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) || y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
  }
  @inline final implicit class JSLE_Function20ToBool[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](private val x: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) && y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) || y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
  }
  @inline final implicit class JSLE_Function21ToBool[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](private val x: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) && y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) || y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
  }
  @inline final implicit class JSLE_Function22ToBool[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](private val x: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) && y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)⇒Boolean): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)⇒Boolean = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) ⇒ x(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) || y(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)
  }

  // Generated by bin/gen-tuple_ext

  @inline final implicit class JSLE_Tuple2[A, B](private val t: (A, B)) extends AnyVal {
    @inline def consume1[U](f: A => U): B = {f(t._1); t._2}
    @inline def consume2[U](f: B => U): A = {f(t._2); t._1}
    @inline def map1[X](f: A => X): (X, B) = (f(t._1), t._2)
    @inline def map2[X](f: B => X): (A, X) = (t._1, f(t._2))
    @inline def put1[X](x: X): (X, B) = (x, t._2)
    @inline def put2[X](x: X): (A, X) = (t._1, x)
  }
  @inline final implicit class JSLE_Tuple3[A, B, C](private val t: (A, B, C)) extends AnyVal {
    @inline def consume1[U](f: A => U): (B, C) = {f(t._1); (t._2, t._3)}
    @inline def consume2[U](f: B => U): (A, C) = {f(t._2); (t._1, t._3)}
    @inline def consume3[U](f: C => U): (A, B) = {f(t._3); (t._1, t._2)}
    @inline def map1[X](f: A => X): (X, B, C) = (f(t._1), t._2, t._3)
    @inline def map2[X](f: B => X): (A, X, C) = (t._1, f(t._2), t._3)
    @inline def map3[X](f: C => X): (A, B, X) = (t._1, t._2, f(t._3))
    @inline def put1[X](x: X): (X, B, C) = (x, t._2, t._3)
    @inline def put2[X](x: X): (A, X, C) = (t._1, x, t._3)
    @inline def put3[X](x: X): (A, B, X) = (t._1, t._2, x)
  }
  @inline final implicit class JSLE_Tuple4[A, B, C, D](private val t: (A, B, C, D)) extends AnyVal {
    @inline def consume1[U](f: A => U): (B, C, D) = {f(t._1); (t._2, t._3, t._4)}
    @inline def consume2[U](f: B => U): (A, C, D) = {f(t._2); (t._1, t._3, t._4)}
    @inline def consume3[U](f: C => U): (A, B, D) = {f(t._3); (t._1, t._2, t._4)}
    @inline def consume4[U](f: D => U): (A, B, C) = {f(t._4); (t._1, t._2, t._3)}
    @inline def map1[X](f: A => X): (X, B, C, D) = (f(t._1), t._2, t._3, t._4)
    @inline def map2[X](f: B => X): (A, X, C, D) = (t._1, f(t._2), t._3, t._4)
    @inline def map3[X](f: C => X): (A, B, X, D) = (t._1, t._2, f(t._3), t._4)
    @inline def map4[X](f: D => X): (A, B, C, X) = (t._1, t._2, t._3, f(t._4))
    @inline def put1[X](x: X): (X, B, C, D) = (x, t._2, t._3, t._4)
    @inline def put2[X](x: X): (A, X, C, D) = (t._1, x, t._3, t._4)
    @inline def put3[X](x: X): (A, B, X, D) = (t._1, t._2, x, t._4)
    @inline def put4[X](x: X): (A, B, C, X) = (t._1, t._2, t._3, x)
  }
  @inline final implicit class JSLE_Tuple5[A, B, C, D, E](private val t: (A, B, C, D, E)) extends AnyVal {
    @inline def map1[X](f: A => X): (X, B, C, D, E) = (f(t._1), t._2, t._3, t._4, t._5)
    @inline def map2[X](f: B => X): (A, X, C, D, E) = (t._1, f(t._2), t._3, t._4, t._5)
    @inline def map3[X](f: C => X): (A, B, X, D, E) = (t._1, t._2, f(t._3), t._4, t._5)
    @inline def map4[X](f: D => X): (A, B, C, X, E) = (t._1, t._2, t._3, f(t._4), t._5)
    @inline def map5[X](f: E => X): (A, B, C, D, X) = (t._1, t._2, t._3, t._4, f(t._5))
    @inline def put1[X](x: X): (X, B, C, D, E) = (x, t._2, t._3, t._4, t._5)
    @inline def put2[X](x: X): (A, X, C, D, E) = (t._1, x, t._3, t._4, t._5)
    @inline def put3[X](x: X): (A, B, X, D, E) = (t._1, t._2, x, t._4, t._5)
    @inline def put4[X](x: X): (A, B, C, X, E) = (t._1, t._2, t._3, x, t._5)
    @inline def put5[X](x: X): (A, B, C, D, X) = (t._1, t._2, t._3, t._4, x)
  }
  @inline final implicit class JSLE_Tuple6[A, B, C, D, E, F](private val t: (A, B, C, D, E, F)) extends AnyVal {
    @inline def map1[X](f: A => X): (X, B, C, D, E, F) = (f(t._1), t._2, t._3, t._4, t._5, t._6)
    @inline def map2[X](f: B => X): (A, X, C, D, E, F) = (t._1, f(t._2), t._3, t._4, t._5, t._6)
    @inline def map3[X](f: C => X): (A, B, X, D, E, F) = (t._1, t._2, f(t._3), t._4, t._5, t._6)
    @inline def map4[X](f: D => X): (A, B, C, X, E, F) = (t._1, t._2, t._3, f(t._4), t._5, t._6)
    @inline def map5[X](f: E => X): (A, B, C, D, X, F) = (t._1, t._2, t._3, t._4, f(t._5), t._6)
    @inline def map6[X](f: F => X): (A, B, C, D, E, X) = (t._1, t._2, t._3, t._4, t._5, f(t._6))
    @inline def put1[X](x: X): (X, B, C, D, E, F) = (x, t._2, t._3, t._4, t._5, t._6)
    @inline def put2[X](x: X): (A, X, C, D, E, F) = (t._1, x, t._3, t._4, t._5, t._6)
    @inline def put3[X](x: X): (A, B, X, D, E, F) = (t._1, t._2, x, t._4, t._5, t._6)
    @inline def put4[X](x: X): (A, B, C, X, E, F) = (t._1, t._2, t._3, x, t._5, t._6)
    @inline def put5[X](x: X): (A, B, C, D, X, F) = (t._1, t._2, t._3, t._4, x, t._6)
    @inline def put6[X](x: X): (A, B, C, D, E, X) = (t._1, t._2, t._3, t._4, t._5, x)
  }
  @inline final implicit class JSLE_Tuple7[A, B, C, D, E, F, G](private val t: (A, B, C, D, E, F, G)) extends AnyVal {
    @inline def map1[X](f: A => X): (X, B, C, D, E, F, G) = (f(t._1), t._2, t._3, t._4, t._5, t._6, t._7)
    @inline def map2[X](f: B => X): (A, X, C, D, E, F, G) = (t._1, f(t._2), t._3, t._4, t._5, t._6, t._7)
    @inline def map3[X](f: C => X): (A, B, X, D, E, F, G) = (t._1, t._2, f(t._3), t._4, t._5, t._6, t._7)
    @inline def map4[X](f: D => X): (A, B, C, X, E, F, G) = (t._1, t._2, t._3, f(t._4), t._5, t._6, t._7)
    @inline def map5[X](f: E => X): (A, B, C, D, X, F, G) = (t._1, t._2, t._3, t._4, f(t._5), t._6, t._7)
    @inline def map6[X](f: F => X): (A, B, C, D, E, X, G) = (t._1, t._2, t._3, t._4, t._5, f(t._6), t._7)
    @inline def map7[X](f: G => X): (A, B, C, D, E, F, X) = (t._1, t._2, t._3, t._4, t._5, t._6, f(t._7))
    @inline def put1[X](x: X): (X, B, C, D, E, F, G) = (x, t._2, t._3, t._4, t._5, t._6, t._7)
    @inline def put2[X](x: X): (A, X, C, D, E, F, G) = (t._1, x, t._3, t._4, t._5, t._6, t._7)
    @inline def put3[X](x: X): (A, B, X, D, E, F, G) = (t._1, t._2, x, t._4, t._5, t._6, t._7)
    @inline def put4[X](x: X): (A, B, C, X, E, F, G) = (t._1, t._2, t._3, x, t._5, t._6, t._7)
    @inline def put5[X](x: X): (A, B, C, D, X, F, G) = (t._1, t._2, t._3, t._4, x, t._6, t._7)
    @inline def put6[X](x: X): (A, B, C, D, E, X, G) = (t._1, t._2, t._3, t._4, t._5, x, t._7)
    @inline def put7[X](x: X): (A, B, C, D, E, F, X) = (t._1, t._2, t._3, t._4, t._5, t._6, x)
  }
  @inline final implicit class JSLE_Tuple8[A, B, C, D, E, F, G, H](private val t: (A, B, C, D, E, F, G, H)) extends AnyVal {
    @inline def map1[X](f: A => X): (X, B, C, D, E, F, G, H) = (f(t._1), t._2, t._3, t._4, t._5, t._6, t._7, t._8)
    @inline def map2[X](f: B => X): (A, X, C, D, E, F, G, H) = (t._1, f(t._2), t._3, t._4, t._5, t._6, t._7, t._8)
    @inline def map3[X](f: C => X): (A, B, X, D, E, F, G, H) = (t._1, t._2, f(t._3), t._4, t._5, t._6, t._7, t._8)
    @inline def map4[X](f: D => X): (A, B, C, X, E, F, G, H) = (t._1, t._2, t._3, f(t._4), t._5, t._6, t._7, t._8)
    @inline def map5[X](f: E => X): (A, B, C, D, X, F, G, H) = (t._1, t._2, t._3, t._4, f(t._5), t._6, t._7, t._8)
    @inline def map6[X](f: F => X): (A, B, C, D, E, X, G, H) = (t._1, t._2, t._3, t._4, t._5, f(t._6), t._7, t._8)
    @inline def map7[X](f: G => X): (A, B, C, D, E, F, X, H) = (t._1, t._2, t._3, t._4, t._5, t._6, f(t._7), t._8)
    @inline def map8[X](f: H => X): (A, B, C, D, E, F, G, X) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7, f(t._8))
    @inline def put1[X](x: X): (X, B, C, D, E, F, G, H) = (x, t._2, t._3, t._4, t._5, t._6, t._7, t._8)
    @inline def put2[X](x: X): (A, X, C, D, E, F, G, H) = (t._1, x, t._3, t._4, t._5, t._6, t._7, t._8)
    @inline def put3[X](x: X): (A, B, X, D, E, F, G, H) = (t._1, t._2, x, t._4, t._5, t._6, t._7, t._8)
    @inline def put4[X](x: X): (A, B, C, X, E, F, G, H) = (t._1, t._2, t._3, x, t._5, t._6, t._7, t._8)
    @inline def put5[X](x: X): (A, B, C, D, X, F, G, H) = (t._1, t._2, t._3, t._4, x, t._6, t._7, t._8)
    @inline def put6[X](x: X): (A, B, C, D, E, X, G, H) = (t._1, t._2, t._3, t._4, t._5, x, t._7, t._8)
    @inline def put7[X](x: X): (A, B, C, D, E, F, X, H) = (t._1, t._2, t._3, t._4, t._5, t._6, x, t._8)
    @inline def put8[X](x: X): (A, B, C, D, E, F, G, X) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7, x)
  }
  @inline final implicit class JSLE_Tuple9[A, B, C, D, E, F, G, H, I](private val t: (A, B, C, D, E, F, G, H, I)) extends AnyVal {
    @inline def map1[X](f: A => X): (X, B, C, D, E, F, G, H, I) = (f(t._1), t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
    @inline def map2[X](f: B => X): (A, X, C, D, E, F, G, H, I) = (t._1, f(t._2), t._3, t._4, t._5, t._6, t._7, t._8, t._9)
    @inline def map3[X](f: C => X): (A, B, X, D, E, F, G, H, I) = (t._1, t._2, f(t._3), t._4, t._5, t._6, t._7, t._8, t._9)
    @inline def map4[X](f: D => X): (A, B, C, X, E, F, G, H, I) = (t._1, t._2, t._3, f(t._4), t._5, t._6, t._7, t._8, t._9)
    @inline def map5[X](f: E => X): (A, B, C, D, X, F, G, H, I) = (t._1, t._2, t._3, t._4, f(t._5), t._6, t._7, t._8, t._9)
    @inline def map6[X](f: F => X): (A, B, C, D, E, X, G, H, I) = (t._1, t._2, t._3, t._4, t._5, f(t._6), t._7, t._8, t._9)
    @inline def map7[X](f: G => X): (A, B, C, D, E, F, X, H, I) = (t._1, t._2, t._3, t._4, t._5, t._6, f(t._7), t._8, t._9)
    @inline def map8[X](f: H => X): (A, B, C, D, E, F, G, X, I) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7, f(t._8), t._9)
    @inline def map9[X](f: I => X): (A, B, C, D, E, F, G, H, X) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, f(t._9))
    @inline def put1[X](x: X): (X, B, C, D, E, F, G, H, I) = (x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
    @inline def put2[X](x: X): (A, X, C, D, E, F, G, H, I) = (t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
    @inline def put3[X](x: X): (A, B, X, D, E, F, G, H, I) = (t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9)
    @inline def put4[X](x: X): (A, B, C, X, E, F, G, H, I) = (t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9)
    @inline def put5[X](x: X): (A, B, C, D, X, F, G, H, I) = (t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9)
    @inline def put6[X](x: X): (A, B, C, D, E, X, G, H, I) = (t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9)
    @inline def put7[X](x: X): (A, B, C, D, E, F, X, H, I) = (t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9)
    @inline def put8[X](x: X): (A, B, C, D, E, F, G, X, I) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9)
    @inline def put9[X](x: X): (A, B, C, D, E, F, G, H, X) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x)
  }

}
