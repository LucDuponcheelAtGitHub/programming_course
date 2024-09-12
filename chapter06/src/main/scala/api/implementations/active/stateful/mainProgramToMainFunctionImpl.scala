package api.implementations.active.stateful

import types.And

import utilities.bind

import api.specification.{MainProgram, MainProgramToMainFunction}

import api.implementations.active.stateful

given mainProgramToMainFunctionImpl[S]
    : MainProgramToMainFunction[stateful.Function[S], S, And[Unit, S]] with

  val MAIN_PROGRAM_TO_MAIN_FUNCTION: MainProgram[stateful.Function[S]] => MainFunction =
    f_u2shu => s => s bind (() bind f_u2shu)
