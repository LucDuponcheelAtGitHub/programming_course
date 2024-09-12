package api.specification.parallel

import types.And

trait Construction[Program[-_, +_]]:

  extension [Z, Y, X](p_z2y: Program[Z, Y])
    infix def PAR_AND(p_z2x: Program[Z, X]): Program[Z, And[Y, X]]
