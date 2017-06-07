import org.virtualized._      // Required Imports to run Spatial Framework
import spatial._

object SwitchVideo extends SpatialApp {
  import IR._
  override val target = targets.DE1  
  type BIT = FixPt[FALSE,_1,_0]
  type UINT2 = FixPt[FALSE,_2,_0]
  type UINT3 = FixPt[FALSE,_3,_0]
  type UINT5 = FixPt[FALSE,_5,_0]
  type UINT6 = FixPt[FALSE,_6,_0]

  @struct class Switches(SW0: BIT, SW1: BIT, SW2: BIT,
                         SW3: BIT, SW4: BIT, SW5: BIT,
                         SW6: BIT, SW7: BIT, SW8: BIT,
                         SW9: BIT)
  @struct case class BGR(B: UINT5, G: UINT6, R: UINT5)
  @struct case class bBgGrR(b: UINT3, B: UINT5, g: UINT2, G: UINT6, r: UINT3, R: UINT5)

  @virtualize 
  def main() {
    val onboardSwitches = target.SliderSwitch   // Slider Switch Target Declaration
    val onboardVideo = target.VideoCamera       // Video Camera Target Declaration
    val outputVideo: Bus = target.VGA                // VGA Target Declaration

    val videoSelect = HostIO[BIT]               // HostIO to allow ARM to read switch
    val switchState = StreamIn[Switches](onboardSwitches) // StreamIn Slider Switch Peripheral
    val inputOnboard = StreamIn[bBgGrR](onboardVideo)     // StreamIn Onboard Video Peripheral
    val output = StreamOut[BGR](outputVideo)    // StreamOut VGA Peripheral


    Accel(*) {
      val switch = switchState.value()          // Retrieve switch states
      videoSelect := switch.SW0.to[BIT]          // Assign video select based on switch state

      val pixel = inputOnboard.value()
      output := BGR(pixel.B.to[UINT5], pixel.G.to[UINT6], pixel.R.to[UINT5])
    }
  }
}
