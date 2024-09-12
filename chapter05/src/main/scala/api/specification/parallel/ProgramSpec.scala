package api.specification.parallel

import types.And

import api.specification.{ProgramSpec => _, *}

trait ProgramSpec[Program[-_, +_]]
    extends api.specification.ProgramSpec[Program],
      parallel.Composition[Program],
      parallel.Construction[Program]:

  extension [Z, Y, X](p_z2y: Program[Z, Y])
    infix def PAR_AND(p_z2x: Program[Z, X]): Program[Z, And[Y, X]] =
      DUPLICATE AND_THEN (p_z2y PAR_WITH p_z2x)
