trait Visualize extends (String => Unit)
object Visualize:
  given Visualize = _ => ()