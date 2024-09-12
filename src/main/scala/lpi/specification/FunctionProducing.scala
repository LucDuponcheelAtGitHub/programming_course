package lpi.specification

private[lpi] type FunctionProducing =
  [Computation[+_]] =>> [Z, Y] =>> Function[Z, Computation[Y]]
