package api.specification

// added for chapter03
import types.And
//

trait ProgramSpec[Program[-_, +_]]
    extends FunctionToProgram[Program],
      SequentialComposition[Program],
      // added for chapter03
      SequentialConstruction[Program]:
  //

  // added for chapter03
  def DUPLICATE[Z]: Program[Z, And[Z, Z]] = IDENTITY SEQ_AND IDENTITY

  extension [Z, Y, X](p_z2y: Program[Z, Y])
    infix def IN(p_zay2x: Program[And[Z, Y], X]): Program[Z, X] =
      IDENTITY SEQ_AND p_z2y AND_THEN p_zay2x
  //
