import java.util.concurrent.atomic.AtomicBoolean

class SpinLock{
  private val b = new AtomicBoolean(false)

  def lock = while (!b.compareAndSet(false,true)) { }

  def unlock = b.set(false)
}
