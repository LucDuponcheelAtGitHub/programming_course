package api.specification

import types.And

trait SequentialConstruction[Program[-_, +_]]:

  extension [Z, Y, X](p_z2y: Program[Z, Y])
    infix def SEQ_AND(p_z2x: => Program[Z, X]): Program[Z, And[Y, X]]
