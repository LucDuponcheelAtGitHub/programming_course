package api.implementations.active.stateful

import types.{And, Or}

import utilities.bind

import api.specification.stateful

import api.implementations.active

given programImpl[S]: stateful.ProgramSpec[S, active.stateful.Function[S]] with

  def FUNCTION_TO_PROGRAM[Z, Y]
      : scala.Predef.Function[Z, Y] => active.stateful.Function[S][Z, Y] = f_z2y =>
    z => s => (z bind f_z2y, s)

  extension [Z, Y, X](f_z2shy: active.stateful.Function[S][Z, Y])
    infix def AND_THEN(
        f_y2shx: => active.stateful.Function[S][Y, X]
    ): active.stateful.Function[S][Z, X] = z =>
      s =>
        val s0: S = s
        val (y, s1) = s0 bind (z bind f_z2shy)
        val (x, s2) = s1 bind (y bind f_y2shx)
        (x, s2)

  extension [Z, Y, X](f_z2shy: active.stateful.Function[S][Z, Y])
    infix def SEQ_AND(
        f_z2shx: => active.stateful.Function[S][Z, X]
    ): active.stateful.Function[S][Z, And[Y, X]] = z =>
      s =>
        val s0: S = s
        val (y, s1) = s0 bind (z bind f_z2shy)
        val (x, s2) = s1 bind (z bind f_z2shx)
        ((y, x), s2)

  extension [Z, Y, X](f_x2shz: => active.stateful.Function[S][X, Z])
    infix def OR(
        f_y2shz: => active.stateful.Function[S][Y, Z]
    ): active.stateful.Function[S][Or[X, Y], Z] = xoy =>
      s =>
        xoy match {
          case Left(x)  => s bind (x bind f_x2shz)
          case Right(y) => s bind (y bind f_y2shz)
        }

  val READ_STATE: active.stateful.Function[S][Unit, S] = _ => s => (s, s)

  val WRITE_STATE: active.stateful.Function[S][S, Unit] = s => _ => ((), s)
