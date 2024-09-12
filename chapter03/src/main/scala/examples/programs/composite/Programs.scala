package examples.programs.composite

// added for chapter03
import types.And
//

import utilities.bind

import api.specification.ProgramSpec

import examples.programs.primitive

trait Programs[Program[-_, +_]: ProgramSpec] extends primitive.Programs[Program]:

  private val summonedProgramSpec: ProgramSpec[Program] = summon[ProgramSpec[Program]]
  import summonedProgramSpec.DUPLICATE

  def shouldBeTrue[Z]: Program[Z, Boolean] = one AND_THEN subtractOne AND_THEN isZero

  def shouldBeFalse[Z]: Program[Z, Boolean] = one AND_THEN subtractOne AND_THEN isOne

  // added for chapter03
  def shouldBeTrueAndFalse[Z]: Program[Z, And[Boolean, Boolean]] =
    shouldBeTrue SEQ_AND shouldBeFalse

  val square: Program[BigInt, BigInt] = DUPLICATE AND_THEN multiply
  //
