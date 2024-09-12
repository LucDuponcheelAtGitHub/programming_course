package examples.applications.active.stateful

import types.And

import utilities.bind

import api.implementations.active.stateful

import stateful.{programImpl, mainProgramToMainFunctionImpl}

import examples.types.Seed

object mains extends examples.mains.stateful.Mains[stateful.Function[Seed], Seed, And[Unit, Seed]]

import mains.twoRandomNaturalsMain


@main def main(): Unit =

  val initialSeed: Seed = 1234567890L

  println(s"initial seed = $initialSeed")

  val (_, finalSeed) = initialSeed bind twoRandomNaturalsMain

  println(s"final seed = $finalSeed")

// @main def main(): Unit =

//   val initialSeed: Seed = 1234567890L

//   initialSeed bind twoRandomNaturalsMain