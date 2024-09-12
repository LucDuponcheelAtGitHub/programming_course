package lpi.implementations.active

import utilities.bind

import api.implementations.active

import lpi.specification.{FunctionProducing, ComputationSpec}

given computationImpl: ComputationSpec[active.Expression] with

  private[lpi] def EXPRESSION_TO_COMPUTATION[Z]: FunctionProducing[active.Expression][Z, Z] =
    identity

  extension [Z, Y](az: active.Expression[Z])
    private[lpi] infix def BIND(
        f_z2ay: => FunctionProducing[active.Expression][Z, Y]
    ): active.Expression[Y] = az bind f_z2ay
