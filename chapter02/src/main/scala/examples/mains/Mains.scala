package examples.mains

import utilities.bind

import api.specification.{ProgramSpec, MainProgramToMainFunction}

import examples.programs.{composite, effectful}

trait Mains[
    Program[-_, +_]
      : ProgramSpec
      : [Program[-_, +_]] =>> MainProgramToMainFunction[Program, A, B],
    A,
    B
] extends composite.Programs[Program],
      effectful.primitive.Programs[Program]:

  private val summonedMainProgramToMainFunction =
    summon[MainProgramToMainFunction[Program, A, B]]
  import summonedMainProgramToMainFunction.{MainFunction, MAIN_PROGRAM_TO_MAIN_FUNCTION}

  val shouldBeTrueMain: MainFunction =
    readBigIntArgument AND_THEN shouldBeTrue AND_THEN writeResult(
      "shouldBeTrue"
    ) bind MAIN_PROGRAM_TO_MAIN_FUNCTION

  val shouldBeFalseMain: MainFunction =
    readBigIntArgument AND_THEN shouldBeFalse AND_THEN writeResult(
      "shouldBeFalse"
    ) bind MAIN_PROGRAM_TO_MAIN_FUNCTION
