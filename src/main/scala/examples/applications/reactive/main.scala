package examples.applications.reactive

import utilities.bind

import api.implementations.reactive

import reactive.{programImpl, mainProgramToMainFunctionImpl}

import examples.mains.Mains

object mains extends Mains[reactive.Function, Unit, Unit]

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
