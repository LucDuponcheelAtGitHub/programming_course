package examples.applications.reactive.parallel

import utilities.bind

import api.implementations.reactive

import reactive.parallel.programImpl

import reactive.mainProgramToMainFunctionImpl

import examples.mains.parallel

object mains extends parallel.Mains[reactive.Function, Unit, Unit]

import mains.fibonacciMain

@main def main(): Unit =
  () bind fibonacciMain
