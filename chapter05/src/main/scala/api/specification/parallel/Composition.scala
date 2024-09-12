package api.specification.parallel

import types.And

trait Composition[Program[-_, +_]]:

  extension [Z, Y, X, W](p_z2x: Program[Z, X])
    infix def PAR_WITH(p_y2w: Program[Y, W]): Program[And[Z, Y], And[X, W]]
