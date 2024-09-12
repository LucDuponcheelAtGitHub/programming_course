package examples.functions.primitive

// added for chapter03
import types.And
//

def oneFunction[Z]: Function[Z, BigInt] = z => 1

val subtractOneFunction: Function[BigInt, BigInt] = z => z - 1

val subtractTwoFunction: Function[BigInt, BigInt] = z => z - 2

val isZeroFunction: Function[BigInt, Boolean] = z => z == 0

val isOneFunction: Function[BigInt, Boolean] = z => z == 1

// added for chapter03
val addFunction: Function[And[BigInt, BigInt], BigInt] = (z, y) => z + y

val multiplyFunction: Function[And[BigInt, BigInt], BigInt] = (z, y) => z * y
//
