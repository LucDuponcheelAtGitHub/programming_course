package utilities

// added for chapter04
import types.{And, Or}
//

extension [Z, Y](z: Z) infix def bind(f_z2y: => Function[Z, Y]): Y = f_z2y apply z

import types.Expression

extension [Z, Y](ez: Expression[Z])
  infix def bindExpression(f_z2ey: => Function[Z, Expression[Y]]): Expression[Y] =
    ez bind f_z2ey

// added for chapter04
def booleanChoiceFunction[Y]: And[Y, Boolean] => Or[Y, Y] = { (y, b) =>
  b match {
    case true  => y bind Left.apply
    case false => y bind Right.apply
  }
}
//
