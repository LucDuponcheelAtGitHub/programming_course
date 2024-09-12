package examples.mains

// added for chapter03
import types.And
//

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
      effectful.primitive.Programs[Program],
      // added for chapter03
      effectful.composite.Programs[Program]:
  //

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

  // added for chapter03

  val addMain: MainFunction =
    readTwoBigIntArguments AND_THEN (add IN writeArgumentAndResult(
      "add"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION

  val multiplyMain: MainFunction =
    readTwoBigIntArguments AND_THEN (multiply IN writeArgumentAndResult(
      "multiply"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION

  def shouldBeTrueAndFalseMain[Z]: MainFunction =
    readBigIntArgument AND_THEN (shouldBeTrueAndFalse IN writeArgumentAndResult(
      "shouldBeTrueAndFalse"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION

  def squareMain[Z]: MainFunction =
    readBigIntArgument AND_THEN (square IN writeArgumentAndResult(
      "square"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION
  //
