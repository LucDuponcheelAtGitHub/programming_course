package examples.applications.active

import utilities.bind

import api.implementations.active

import active.{programImpl, mainProgramToMainFunctionImpl}

import examples.mains.Mains

object mains extends Mains[active.Function, Unit, Unit]

import mains.{
  // shouldBeTrueMain,
  // shouldBeFalseMain,
  // added for chapter03
  addMain,
  multiplyMain,
  shouldBeTrueAndFalseMain,
  squareMain,
  //
  // added for chapter04
  fibonacciMain,
  factorialMain
  //
}

@main def main(): Unit =

  // () bind shouldBeTrueMain

  // () bind shouldBeFalseMain

  // added for chapter03
  // () bind addMain

  // () bind multiplyMain

  // () bind shouldBeTrueAndFalseMain

  // () bind squareMain
  //

  // added for chapter04
  () bind fibonacciMain

  () bind factorialMain
  //
