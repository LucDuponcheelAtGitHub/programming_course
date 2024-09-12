package examples.programs.parallel.composite

import utilities.bind

import api.specification.parallel

import examples.programs.primitive

trait Programs[Program[-_, +_]: parallel.ProgramSpec]
    extends primitive.Programs[Program]:

  val summonedParallelProgramSpec: parallel.ProgramSpec[Program] =
    summon[parallel.ProgramSpec[Program]]
  import summonedParallelProgramSpec.IF

  val fibonacci: Program[BigInt, BigInt] =
    IF(isZero) {
      one
    } ELSE {
      IF(isOne) {
        one
      } ELSE {
        (subtractOne AND_THEN fibonacci) PAR_AND (subtractTwo AND_THEN fibonacci) AND_THEN add
      }
    }
