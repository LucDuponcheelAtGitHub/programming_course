package api.specification.stateful

import utilities.bind

import api.specification.{ProgramSpec => _, *}

trait ProgramSpec[S, Program[-_, +_]]
    extends api.specification.ProgramSpec[Program],
      Manipulation[S, Program]:

  def MODIFY_STATE_WITH[Z]: Function[S, S] => Program[Z, Unit] = f_s2s =>
    { (z: Z) => () } bind FUNCTION_TO_PROGRAM AND_THEN
      READ_STATE AND_THEN
      (f_s2s bind FUNCTION_TO_PROGRAM) AND_THEN
      WRITE_STATE

  def READ_STATE_MODIFIED_WITH[Z]: Function[S, S] => Program[Z, S] = f_s2s =>
    f_s2s bind MODIFY_STATE_WITH AND_THEN READ_STATE
