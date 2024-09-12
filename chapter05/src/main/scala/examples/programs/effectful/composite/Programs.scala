package examples.programs.effectful.composite

import types.And

import api.specification.ProgramSpec

import examples.programs.effectful

trait Programs[Program[-_, +_]: ProgramSpec] extends effectful.primitive.Programs[Program]:

  val readTwoBigIntArguments: Program[Unit, And[BigInt, BigInt]] =
    readBigIntArgument SEQ_AND readBigIntArgument
