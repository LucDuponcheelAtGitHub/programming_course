package api.specification

import types.Or

trait Condition[Program[-_, +_]]:

  extension [Z, Y, X](p_x2z: => Program[X, Z])
    infix def OR(p_y2z: => Program[Y, Z]): Program[Or[X, Y], Z]
