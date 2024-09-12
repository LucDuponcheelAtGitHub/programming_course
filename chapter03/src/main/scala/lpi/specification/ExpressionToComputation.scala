package lpi.specification

private[lpi] trait ExpressionToComputation[Computation[+_]]:

  private[lpi] def EXPRESSION_TO_COMPUTATION[Z]: FunctionProducing[Computation][Z, Z]
