package examples.mains.stateful

import utilities.bind

import api.specification.{MainProgram, MainProgramToMainFunction}

import api.specification.stateful

import examples.types.Seed

import examples.programs.stateful.composite

import examples.programs.effectful

trait Mains[
    Program[-_, +_]
      : [Program[-_, +_]] =>> stateful.ProgramSpec[Seed, Program]
      : [Program[-_, +_]] =>> MainProgramToMainFunction[Program, A, B],
    A,
    B
] extends composite.Programs[Program],
      effectful.primitive.Programs[Program],
      effectful.composite.Programs[Program]:

  private val summonedMainProgramToMainFunction =
    summon[MainProgramToMainFunction[Program, A, B]]
  import summonedMainProgramToMainFunction.{MainFunction, MAIN_PROGRAM_TO_MAIN_FUNCTION}

  val twoRandomNaturalsMain: MainFunction =
    readUnitArgument AND_THEN (twoRandomNaturals IN writeArgumentAndResult(
      "twoRandomNaturals"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION
