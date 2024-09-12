package examples.applications.reactive.parallel.logging

import utilities.bind

import api.implementations.reactive

import reactive.parallel.logging.programImpl

import reactive.mainProgramToMainFunctionImpl

import examples.mains.parallel

object mains extends parallel.logging.Mains[reactive.Function, Unit, Unit]

import mains.fibonacciMain

@main def main(): Unit =
  () bind fibonacciMain
