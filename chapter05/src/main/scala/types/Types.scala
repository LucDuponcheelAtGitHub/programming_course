package types

type Expression = [Z] =>> Z

type Callback = [C[+_]] =>> [Z] =>> Function[C[Z], Unit]

type CallbackHandler = [C[+_]] =>> [Z] =>> Function[Callback[C][Z], Unit]

// added for chapter03
type And = [Z, Y] =>> Tuple2[Z, Y]
//

// added for chapter04
type Or = [Z, Y] =>> Either[Z, Y]
//