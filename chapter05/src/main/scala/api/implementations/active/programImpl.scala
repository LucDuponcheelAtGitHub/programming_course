package api.implementations.active

import api.specification.ProgramSpec

import api.implementations.active

import lpi.implementations.generic.computationSpecToProgramImpl

import lpi.implementations.active.computationImpl

given programImpl: ProgramSpec[active.Function] = computationSpecToProgramImpl
