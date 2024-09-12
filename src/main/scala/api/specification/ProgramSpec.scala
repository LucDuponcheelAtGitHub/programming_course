package api.specification

// added for chapter03
import types.And
//

// added for chapter04
import utilities.{bind, booleanChoiceFunction}
//

trait ProgramSpec[Program[-_, +_]]
    extends FunctionToProgram[Program],
      SequentialComposition[Program],
      // added for chapter03
      SequentialConstruction[Program],
      // added for chapter04
      Condition[Program]:
  //

  // added for chapter03
  def DUPLICATE[Z]: Program[Z, And[Z, Z]] = IDENTITY SEQ_AND IDENTITY

  extension [Z, Y, X](p_z2y: Program[Z, Y])
    infix def IN(p_zay2x: Program[And[Z, Y], X]): Program[Z, X] =
      IDENTITY SEQ_AND p_z2y AND_THEN p_zay2x
  //

  // added for chapter04
  private[specification] trait Else[Y, Z]:
    def ELSE(ff_y2z: Program[Y, Z]): Program[Y, Z]

  private[specification] trait Apply[Y, Z]:
    def apply(tf_y2z: Program[Y, Z]): Else[Y, Z]

  def IF[Y, Z](f_y2b: Program[Y, Boolean])(tf_y2z: Program[Y, Z]): Else[Y, Z] =
    new Else[Y, Z]:
      def ELSE(ff_y2z: Program[Y, Z]): Program[Y, Z] =
        f_y2b IN {
          booleanChoiceFunction bind FUNCTION_TO_PROGRAM
        } AND_THEN {
          tf_y2z OR ff_y2z
        }
  //
