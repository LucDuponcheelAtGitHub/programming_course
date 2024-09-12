package lpi.specification

private[lpi] trait Bind[Computation[+_]]:

  extension [Z, Y](cz: Computation[Z])
    private[lpi] infix def BIND(f_z2cy: => FunctionProducing[Computation][Z, Y]): Computation[Y]
