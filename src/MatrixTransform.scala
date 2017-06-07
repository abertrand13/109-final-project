import org.virtualized._
import spatial._

object MatrixTransform extends SpatialApp {
  import IR._
  
   
  @virtualize
  def main() = { 
    val x = ArgIn[Float]
    val y = ArgIn[Float]
    val z = ArgIn[Float]

    val qR = ArgIn[Float] 
    val qI = ArgIn[Float]
    val qJ = ArgIn[Float]
    val qK = ArgIn[Float]

    val inX = args(0).to[Float]
    val inY = args(1).to[Float]
    val inZ = args(2).to[Float]

    val inQR = args(3).to[Float]
    val inQI = args(4).to[Float]
    val inQJ = args(5).to[Float]
    val inQK = args(6).to[Float]

    setArg(x, inX)
    setArg(y, inY)
    setArg(z, inZ)

    setArg(qR, inQR)
    setArg(qI, inQI)
    setArg(qJ, inQJ)
    setArg(qK, inQK)
    
    val a = ArgOut[Float]
    val b = ArgOut[Float]
    val c = ArgOut[Float]

    val DIM = 4

    val forearmLength = 10
    
    Accel {
      val outX = Reg[Float]
      val outY = Reg[Float]
      val outZ = Reg[Float]

      val transX = Reg[Float]
      val transY = Reg[Float]
      val transZ = Reg[Float]
      
      // create coordinate vector 
      // val coordVec = SRAM[Float](DIM)
      // fill with coordinate values
      /*Sequential {
        coordVec(0) = x
        coordVec(1) = y
        coordVec(2) = z
        coordVec(3) = 1
      }*/

       
      // vector representing the translation from the wrist to elbow 
      val translateVec = SRAM[Float](DIM)
      Sequential {
        // replace with real values at some point 
        translateVec(0) = 10
        translateVec(1) = 1
        translateVec(2) = 1
      }

      // convert quaternion to rotation matrix
      val rotationMat = SRAM[Float](DIM, DIM)
      Sequential {
        rotationMat(0,0) = 1 - 2 * (qJ*qJ + qK*qK)
        rotationMat(0,1) = 2 * (qI*qJ - qK*qR)
        rotationMat(0,2) = 2 * (qI*qK + qJ*qR)
        
        rotationMat(1,0) = 2 * (qI*qJ + qK*qR)
        rotationMat(1,1) = 1 - 2 * (qI*qI + qK*qK)
        rotationMat(1,2) = 2 * (qJ*qK - qI*qR)

        rotationMat(2,0) = 2 * (qI*qK - qJ*qR)
        rotationMat(2,1) = 2 * (qJ*qK + qI*qR)
        rotationMat(2,2) = 1 - 2 * (qI*qI + qJ*qJ)
      }

      // apply the rotation matrix to the translation vector
      Reduce(transX)(DIM by 1){ i => 
        translateVec(i) * rotationMat(0, i)
      }{_+_}

      Reduce(transY)(DIM by 1){ i =>
        translateVec(i) * rotationMat(1, i)
      }{_+_}
      
      Reduce(transZ)(DIM by 1){ i =>
        translateVec(i) * rotationMat(2, i)
      }{_+_}

      // we now have a simple translation to perform on the wrist
      // position to get the elbow position
      outX := x + transX.value
      outY := y + transY.value
      outZ := z + transZ.value

         
      // create transformation matrix 
      /*val transformMat = SRAM[Float](DIM, DIM) 
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
      }*/
 
      
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
