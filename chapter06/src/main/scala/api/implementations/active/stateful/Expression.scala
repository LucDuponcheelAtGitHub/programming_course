package api.implementations.active.stateful

import types.StateHandler

import api.implementations.active

type Expression = [S] =>> [Z] =>> StateHandler[S][active.Expression[Z]]
