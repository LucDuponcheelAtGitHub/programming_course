package examples.programs.composite

import utilities.bind

import api.specification.ProgramSpec

import examples.programs.primitive

trait Programs[Program[-_, +_]: ProgramSpec] extends primitive.Programs[Program]:

  def shouldBeTrue[Z]: Program[Z, Boolean] = one AND_THEN subtractOne AND_THEN isZero

  def shouldBeFalse[Z]: Program[Z, Boolean] = one AND_THEN subtractOne AND_THEN isOne
