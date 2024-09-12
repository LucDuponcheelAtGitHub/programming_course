package examples.functions.primitive

def oneFunction[Z]: Function[Z, BigInt] = z => 1

val subtractOneFunction: Function[BigInt, BigInt] = z => z - 1

val subtractTwoFunction: Function[BigInt, BigInt] = z => z - 2

val isZeroFunction: Function[BigInt, Boolean] = z => z == 0

val isOneFunction: Function[BigInt, Boolean] = z => z == 1
