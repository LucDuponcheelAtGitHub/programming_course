package api.specification

import api.specification.MainProgram

trait MainProgramToMainFunction[Program[-_, +_], A, B]:

  type MainFunction = Function[A, B]

  val MAIN_PROGRAM_TO_MAIN_FUNCTION: MainProgram[Program] => MainFunction
