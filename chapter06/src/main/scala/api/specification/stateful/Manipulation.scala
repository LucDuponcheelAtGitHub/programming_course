package api.specification.stateful

trait Manipulation[S, Program[-_, +_]]:

  val READ_STATE: Program[Unit, S]

  val WRITE_STATE: Program[S, Unit]
