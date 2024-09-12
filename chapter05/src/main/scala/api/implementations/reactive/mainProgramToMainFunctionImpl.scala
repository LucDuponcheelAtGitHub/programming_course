package api.implementations.reactive

import utilities.bind

import api.specification.{MainProgram, MainProgramToMainFunction}

import api.implementations.reactive

given mainProgramToMainFunctionImpl: MainProgramToMainFunction[reactive.Function, Unit, Unit]
with

  val MAIN_PROGRAM_TO_MAIN_FUNCTION: MainProgram[reactive.Function] => MainFunction =
    f_u2cbhu => u => identity[Unit] bind (u bind f_u2cbhu)
