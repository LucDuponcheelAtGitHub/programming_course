package examples.functions.effectful

import scala.io.StdIn.readInt

// added for chapter03
import types.And
//

val readBigIntArgumentFunction: Function[Unit, BigInt] = u =>
  println()
  println("please type a BigInt argument")
  readInt()

def writeResultFunction[Z](programName: String): Function[Z, Unit] = i =>
  println(s"the result of applying $programName is $i")

// added for chapter03
def writeArgumentAndResultFunction[Z, Y](programName: String): And[Z, Y] => Unit =
  (z, y) => println(s"the result of binding argument $z to $programName is $y")
//
