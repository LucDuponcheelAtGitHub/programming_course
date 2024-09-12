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

// added for chapter06
val isNotNegativeFunction: BigInt => Boolean = i => i >= 0

val negateFunction: BigInt => BigInt = i => -i

val moduloMillionFunction: BigInt => BigInt = i => i % 1000000

import examples.types.Seed

val seed2randomBigIntFunction: Seed => BigInt = seed => BigInt((seed >>> 16).toInt)

val randomSeedModifierFunction: Seed => Seed = seed =>
  (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
//
