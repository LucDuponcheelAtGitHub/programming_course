package api.implementations.reactive

import api.specification.ProgramSpec

import api.implementations.reactive

import lpi.implementations.generic.computationSpecToProgramImpl

import lpi.implementations.reactive.computationImpl

given programImpl: ProgramSpec[reactive.Function] = computationSpecToProgramImpl
