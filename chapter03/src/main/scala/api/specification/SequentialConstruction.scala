package api.specification

import types.And

trait SequentialConstruction[Program[-_, +_]]:

  extension [Z, Y, X](`z>-->y`: Program[Z, Y])
    infix def SEQ_AND(`z>-->x`: Program[Z, X]): Program[Z, And[Y, X]]
