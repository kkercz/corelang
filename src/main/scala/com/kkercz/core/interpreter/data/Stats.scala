package com.kkercz.core.interpreter.data

case class Stats(steps: Int) {

  def incrSteps(): Stats = Stats(steps + 1)

}

object Stats {
  def apply(): Stats = new Stats(0)
}