package core.interpreter.ti.data

case class Stats(steps: Int, reductions: Int, gcStats: GcStats) {

  def incrSteps(): Stats = Stats(steps + 1, reductions, gcStats)
  def incrReductions(): Stats = Stats(steps, reductions + 1, gcStats)
  def updateGcStats(reclaimedSpace: Int, elapsedTime: Long): Stats = Stats(steps, reductions, GcStats(
    gcStats.reclaimedSpace + reclaimedSpace,
    gcStats.elapsedTimeMs + elapsedTime,
    gcStats.timesRun + 1
  ))
}

case class GcStats(reclaimedSpace: Int, elapsedTimeMs: Long, timesRun: Int)

object Stats {
  def apply(): Stats = new Stats(0, 0, GcStats(0, 0, 0))
}