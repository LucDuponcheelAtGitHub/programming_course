package api.specification

import utilities.bind

trait FunctionToProgram[Program[-_, +_]]:

  def FUNCTION_TO_PROGRAM[Z, Y]: Function[Z, Y] => Program[Z, Y]

  def IDENTITY[Z]: Program[Z, Z] = identity[Z] bind FUNCTION_TO_PROGRAM
