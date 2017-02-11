/**
  * Created by Rajesh Sampathkumar  on 2/11/2017.
  */

package DataAnalysis
import java.util.Random

class dataGen(cont: Boolean = true, cumul: Boolean = false, dist: String, shape: Double, size : Int, loc: Double, scale: Double) {

  val distType = cont
  val cumulative = cumul
  val distPDF = dist
  val sampleSize = size
  val shp = shape
  val locParam = loc
  val scaleParam = scale

  val dataGenResult = genDistData(distPDF, shp, size, locParam, scaleParam)

  def genNorm(loc: Double, scale: Double, size: Int): Array[Double] = {

    def gaussian (x: Double) : Double = (1/scale*Math.sqrt(2*Math.PI)) * Math.exp(-Math.pow(x - loc, 2) / (2*scale*scale))

    val independentVar :Array[Double] = Array.fill[Double](size)(Math.random())
    return independentVar.map(x => gaussian(x))

  }


  def genWeibull(shp: Double, loc: Double, scale: Double, size: Int): Array[Double] = {
    def weibull (x: Double) : Double = (shp/scale) * Math.pow( (x - loc) / scale, (shp - 1)) * Math.exp( -Math.pow( (x-loc)/scale, shp))

    val independentVar :Array[Double] = Array.fill[Double](size)(Math.random())
    return independentVar.map(x => weibull(x))
  }

  def genLogNorm(loc: Double, scale: Double, size: Int): Array[Double] = {
    def lognormal (x: Double) : Double = (1/ x*scale*Math.sqrt(2*Math.PI)) * Math.exp( - Math.pow(Math.log(x),2) / 2 * scale*scale)

    val independentVar :Array[Double] = Array.fill[Double](size)(Math.random())
    return independentVar.map(x => lognormal(x))
  }

  def genDistData(distType: String, shp: Double, size:Int, loc:Double, scale:Double ): Array[Double] = distType match {

    case "normal" => return genNorm(loc, scale, size)
      
    case "lognormal" => return genLogNorm(loc, scale, size)
      
    case "weibull" => return genWeibull(shp, loc, scale, size)
  }


}
