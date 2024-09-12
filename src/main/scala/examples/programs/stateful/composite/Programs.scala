package examples.programs.stateful.composite

import types.And

import utilities.bind

import api.specification.stateful

import examples.types.Seed

import examples.functions.primitive.randomSeedModifierFunction

import examples.programs.primitive

trait Programs[Program[-_, +_]: [Program[-_, +_]] =>> stateful.ProgramSpec[Seed, Program]]
    extends primitive.Programs[Program],
      examples.programs.stateful.primitive.Programs[Program]:

  private val summonedStatefulProgramSpec: stateful.ProgramSpec[Seed, Program] =
    summon[stateful.ProgramSpec[Seed, Program]]
  import summonedStatefulProgramSpec.{IDENTITY, IF}

  def randomBigInt[Z]: Program[Z, BigInt] = readModifiedSeed AND_THEN seed2randomBigInt

  val negateIfNegative: Program[BigInt, BigInt] =
    IF(isNotNegative) {
      IDENTITY
    } ELSE {
      negate
    }

  def randomNatural[Z]: Program[Z, BigInt] =
    randomBigInt AND_THEN negateIfNegative AND_THEN moduloMillion

  def twoRandomNaturals[Z]: Program[Z, And[BigInt, BigInt]] =
    randomNatural SEQ_AND randomNatural
