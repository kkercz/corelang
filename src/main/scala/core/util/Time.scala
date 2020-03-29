package core.util

case object Time {
  def measure[T](block: => T): (T, Long) = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    (result, t1 - t0)
  }
}
