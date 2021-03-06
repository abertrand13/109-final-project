import spatial._
import org.virtualized._
import spatial.targets.DE1

object StreamingSobel extends SpatialApp { 
  import IR._

  override val target = DE1

  val Kh = 3
  val Kw = 3
  val Cmax = 128

  type Int16 = FixPt[TRUE,_16,_0]
  type UInt8 = FixPt[FALSE,_8,_0]
  type UInt5 = FixPt[FALSE,_5,_0]
  type UInt6 = FixPt[FALSE,_6,_0]
  @struct case class Pixel24(b: UInt8, g: UInt8, r: UInt8)
  @struct case class Pixel16(b: UInt5, g: UInt6, r: UInt5)

  @virtualize
  def convolveVideoStream(rows: Int, cols: Int): Unit = {
    val R = ArgIn[Int]
    val C = ArgIn[Int]
    setArg(R, rows)
    setArg(C, cols)

    val imgIn  = StreamIn[Pixel24](target.VideoCamera)
    val imgOut = StreamOut[Pixel16](target.VGA)

    Accel {
      val kh = RegFile[Int16](Kh, Kw)
      val kv = RegFile[Int16](Kh, Kw)

      Pipe{kh(0,0) = 1
        kh(1,0) = 2 
        kh(2,0) = 1
        kh(0,1) = 0
        kh(1,1) = 0
        kh(2,1) = 0
        kh(0,2) = -1
        kh(1,2) = -2
        kh(2,2) = -1
        kv(0,0) = 1 
        kv(0,1) = 2
        kv(0,2) = 1
        kv(1,0) = 0
        kv(1,1) = 0
        kv(1,2) = 0
        kv(2,0) = -1
        kv(2,1) = -2
        kv(2,2) = -1
      }

      val sr = RegFile[Int16](Kh, Kw)
      val fifoIn = FIFO[Int16](128)
      val fifoOut = FIFO[Int16](128)
      val lb = LineBuffer[Int16](Kh, Cmax)

      Stream(*) { _ =>
        val pixel = imgIn.value()          // Retrieve 24 bit pixel
        val grayscale = (pixel.b.to[Int16] + pixel.g.to[Int16] + pixel.r.to[Int16])/3 // Grayscale average
        fifoIn.enq(grayscale)             // Enqueue

        Foreach(R by 1, C by 1){ (row,col) =>
          lb.enq(fifoIn.deq())            // Enqueue into line buffer

          Foreach(Kh by 1 par Kw){ i => sr(i, *) <<= lb(i, col)}    // Create window
          
          val horz = Reduce(Reg[Int16])(Kh by 1, Kw by 1){ (i,j) => // Horizontal Convolution
            val number = mux(row < Kh-1 || col < Kw-1, 0.to[Int16], sr(i,j))
            number * kh(i,j)
          }{_+_}

          val vert = Reduce(Reg[Int16])(Kh by 1, Kw by 1){ (i,j) => // Vertical Convoution
            val number = mux(row < Kh-1 || col < Kw-1, 0.to[Int16], sr(i,j))
            number * kv(i,j)
          }{_+_}
          
          fifoOut.enq(abs(horz.value) + abs(vert.value))        // Enqueue absolute value of convolution sum
        }

        val pixelOut = fifoOut.deq();                           // Dequeue and send to outbound port
        // Interested in green channel only. 
        imgOut := Pixel16(pixelOut(10::6).as[UInt5], pixelOut(10::5).as[UInt6], pixelOut(10::6).as[UInt5]);
      }
      ()
    }
  }

  @virtualize
  def main() {
    val R = args(0).to[Int]
    val C = Cmax
    convolveVideoStream(R, C)
  }
}
