package api.specification

trait ProgramSpec[Program[-_, +_]]
    extends FunctionToProgram[Program],
      SequentialComposition[Program]
