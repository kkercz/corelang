package core.ast.prettyprint

import core.ast.Name

trait HasName[T] {
  def getName(t: T): String
}

object HasName {
  implicit val nameIsName: HasName[Name] = (name: Name) => name
}
