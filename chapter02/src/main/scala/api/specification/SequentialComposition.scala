package api.specification

trait SequentialComposition[Program[-_, +_]]:

  extension [Z, Y, X](p_z2y: Program[Z, Y])
    infix def AND_THEN(p_y2x: => Program[Y, X]): Program[Z, X]
