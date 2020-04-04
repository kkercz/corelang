package core.interpreter

import core.ast.Name

package object data {

  type Address = Int

  type Stack = List[Address]
  type Dump = List[Stack]
  type Globals = Map[Name, Address]
}
