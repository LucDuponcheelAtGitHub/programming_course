package examples.programs.effectful.primitive

// added for chapter03
import types.And
//

import utilities.bind

import api.specification.ProgramSpec

import examples.functions.effectful.{
  readBigIntArgumentFunction,
  writeResultFunction,
  // added for chapter03
  writeArgumentAndResultFunction
  //
}

trait Programs[Program[-_, +_]: ProgramSpec]:

  private val summonedProgramSpec = summon[ProgramSpec[Program]]
  import summonedProgramSpec.FUNCTION_TO_PROGRAM

  val readBigIntArgument: Program[Unit, BigInt] =
    readBigIntArgumentFunction bind FUNCTION_TO_PROGRAM

  def writeResult[Z](programName: String): Program[Z, Unit] =
    programName bind writeResultFunction bind FUNCTION_TO_PROGRAM

  // added for chapter03
  def writeArgumentAndResult[Z, Y](programName: String): Program[And[Z, Y], Unit] =
    programName bind writeArgumentAndResultFunction bind FUNCTION_TO_PROGRAM
  //
