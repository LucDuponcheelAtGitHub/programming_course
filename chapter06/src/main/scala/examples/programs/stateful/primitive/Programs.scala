package examples.programs.stateful.primitive

import utilities.bind

import api.specification.stateful

import examples.types.Seed

import examples.functions.primitive.randomSeedModifierFunction

import examples.programs.primitive

trait Programs[Program[-_, +_]: [Program[-_, +_]] =>> stateful.ProgramSpec[Seed, Program]]
    extends primitive.Programs[Program]:

  private val summonedStatefulProgramSpec: stateful.ProgramSpec[Seed, Program] =
    summon[stateful.ProgramSpec[Seed, Program]]
  import summonedStatefulProgramSpec.READ_STATE_MODIFIED_WITH

  def readModifiedSeed[Z]: Program[Z, Seed] =
    randomSeedModifierFunction bind READ_STATE_MODIFIED_WITH