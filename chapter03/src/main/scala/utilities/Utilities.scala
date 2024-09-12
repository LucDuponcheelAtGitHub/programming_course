package utilities

extension [Z, Y](z: Z) infix def bind(f_z2y: => Function[Z, Y]): Y = f_z2y apply z

import types.Expression

extension [Z, Y](ez: Expression[Z])
  infix def bindExpression(f_z2ey: => Function[Z, Expression[Y]]): Expression[Y] =
    ez bind f_z2ey
