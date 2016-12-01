package japgolly.microlibs

package object config {

  private[config] def keyModTS(f: Key => Key): String => String = s => f(Key(s)).value
  private[config] def keyModFS(f: String => String): Key => Key = k => Key(f(k.value))

}
