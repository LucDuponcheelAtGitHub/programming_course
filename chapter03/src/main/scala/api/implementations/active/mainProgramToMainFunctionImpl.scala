package api.implementations.active

import utilities.bind

import api.specification.{MainProgram, MainProgramToMainFunction}

import api.implementations.active

given mainProgramToMainFunctionImpl: MainProgramToMainFunction[active.Function, Unit, Unit]
with

  val MAIN_PROGRAM_TO_MAIN_FUNCTION: MainProgram[active.Function] => MainFunction =
    f_u2u => u => u bind f_u2u
