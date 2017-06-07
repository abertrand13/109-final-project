import org.virtualized._
import spatial._

object MatrixTransform extends SpatialApp {
  import IR._
  
   
  @virtualize
  def main() = {
    val x = ArgIn[Float]
    val y = ArgIn[Float]
    val z = ArgIn[Float]

    val thetaX = ArgIn[Float]
    val thetaY = ArgIn[Float]
    val thetaZ = ArgIn[Float]

    val inX = args(0).to[Float]
    val inY = args(1).to[Float]
    val inZ = args(2).to[Float]

    setArg(x, inX)
    setArg(y, inY)
    setArg(z, inZ)
    
    val a = ArgOut[Float]
    val b = ArgOut[Float]
    val c = ArgOut[Float]

    // val sineX = Math.sin(x).to[Float]

    val DIM = 4

    val forearmLength = 10
    
    Accel {
      val outX = Reg[Float]
      val outY = Reg[Float]
      val outZ = Reg[Float]
      
      // create coordinate vector 
      val coordVec = SRAM[Float](DIM)
      // fill with coordinate values
      Sequential {
        coordVec(0) = x
        coordVec(1) = y
        coordVec(2) = z
        coordVec(3) = 1
      }
         
      // create transformation matrix 
      val transformMat = SRAM[Float](DIM, DIM) 
      // fill matrix with proper values 
      Sequential {
        // identity part of the matrix 
        transformMat(0,0) = 1.to[Float]
        transformMat(1,1) = 1.to[Float] 
        transformMat(2,2) = 1.to[Float] 
        transformMat(3,3) = 1.to[Float]
        
        // transformation part of matrix  
        transformMat(0,3) = 1.to[Float]
        transformMat(1,3) = 2.to[Float]
        transformMat(2,3) = 3.to[Float]
        
        // zeros everywhere else (comment out for synthesis if you want) 
        transformMat(0,1) = 0.to[Float]
        transformMat(0,2) = 0.to[Float]
        transformMat(1,0) = 0.to[Float] 
        transformMat(1,2) = 0.to[Float]
        transformMat(2,0) = 0.to[Float]
        transformMat(2,1) = 0.to[Float]
        transformMat(3,0) = 0.to[Float]
        transformMat(3,1) = 0.to[Float]
        transformMat(3,2) = 0.to[Float]
      }
 
      // apply the transformation matrix to the input coordinates
      Reduce(outX)(DIM by 1){ i => 
        coordVec(i) * transformMat(0, i)
      }{_+_}

      Reduce(outY)(DIM by 1){ i =>
        coordVec(i) * transformMat(1, i)
      }{_+_}
      
      Reduce(outZ)(DIM by 1){ i =>
        coordVec(i) * transformMat(2, i)
      }{_+_}

      a := outX.value
      // a := x * 1.5
      b := outY.value
      c := outZ.value
    }
    
    println("Resulting Coords: (" + getArg(a) + "," + getArg(b) + "," + getArg(c) + ")")

  }

  /*@virtualize 
  def makeVector4(x:Float, y:Float, z:Float):Vector4[Float] = {
    Vector.ZeroFirst(x, y, z, 1.to[Float])
  }*/
}
