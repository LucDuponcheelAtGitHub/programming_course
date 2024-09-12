package examples.applications.reactive

import utilities.bind

import api.implementations.reactive

import reactive.{programImpl, mainProgramToMainFunctionImpl}

import examples.mains.Mains

object mains extends Mains[reactive.Function, Unit, Unit]

import mains.{shouldBeTrueMain, shouldBeFalseMain}

@main def main(): Unit =

  () bind shouldBeTrueMain

  () bind shouldBeFalseMain
