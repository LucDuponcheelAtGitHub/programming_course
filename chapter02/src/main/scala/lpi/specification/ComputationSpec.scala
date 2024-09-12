package lpi.specification

private[lpi] trait ComputationSpec[Computation[+_]]
    extends ExpressionToComputation[Computation],
      Bind[Computation]
