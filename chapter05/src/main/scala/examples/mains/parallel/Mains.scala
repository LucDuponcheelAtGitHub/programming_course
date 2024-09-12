package examples.mains.parallel

import utilities.bind

import api.specification.{MainProgram, MainProgramToMainFunction}

import api.specification.parallel

import examples.programs.parallel.composite

import examples.programs.effectful

trait Mains[
    Program[-_, +_]
      : parallel.ProgramSpec
      : [Program[-_, +_]] =>> MainProgramToMainFunction[Program, A, B],
    A,
    B
] extends composite.Programs[Program],
      effectful.primitive.Programs[Program],
      effectful.composite.Programs[Program]:

  private val summonedMainProgramToMainFunction =
    summon[MainProgramToMainFunction[Program, A, B]]
  import summonedMainProgramToMainFunction.{MainFunction, MAIN_PROGRAM_TO_MAIN_FUNCTION}

  val fibonacciMain: MainFunction =
    readBigIntArgument AND_THEN (fibonacci IN writeArgumentAndResult(
      "fibonacci"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION
