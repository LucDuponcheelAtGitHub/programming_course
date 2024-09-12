package api.specification

type MainProgram = [Program[-_, +_]] =>> Program[Unit, Unit]
