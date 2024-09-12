package examples.applications.active

import utilities.bind

import api.implementations.active

import active.{programImpl, mainProgramToMainFunctionImpl}

import examples.mains.Mains

object mains extends Mains[active.Function, Unit, Unit]

import mains.{shouldBeTrueMain, shouldBeFalseMain}

@main def main(): Unit =

  () bind shouldBeTrueMain

  () bind shouldBeFalseMain
