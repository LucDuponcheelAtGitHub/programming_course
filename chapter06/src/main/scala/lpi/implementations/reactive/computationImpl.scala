package lpi.implementations.reactive

import utilities.bind

import api.implementations.reactive

import lpi.specification.{FunctionProducing, ComputationSpec}

given computationImpl: ComputationSpec[reactive.Expression] with

  private[lpi] def EXPRESSION_TO_COMPUTATION[Z]
      : FunctionProducing[reactive.Expression][Z, Z] = z => cbz => z bind cbz

  extension [Z, Y](cbhz: reactive.Expression[Z])
    private[lpi] infix def BIND(
        f_z2cbhy: FunctionProducing[reactive.Expression][Z, Y]
    ): reactive.Expression[Y] = cby => { (z: Z) => cby bind (z bind f_z2cbhy) } bind cbhz
