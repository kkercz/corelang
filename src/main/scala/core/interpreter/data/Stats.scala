package core.interpreter.data

case class Stats(steps: Int, reductions: Int) {

  def incrSteps(): Stats = Stats(steps + 1, reductions)
  def incrReductions(): Stats = Stats(steps, reductions + 1)

}

object Stats {
  def apply(): Stats = new Stats(0, 0)
}