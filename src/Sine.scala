import org.virtualized._
import spatial._

object Sine extends SpatialApp {
  import IR._
  
  @virtualize
  def main() = {
    val deg = 45
    val res = Math.sin(deg)

    val q = ArgOut[Double]
    
    Accel {
      q := res
    }
    
    println("Result: " + getArg(q))


  }
}
