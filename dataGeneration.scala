/**
  * Created by Rajesh Sampathkumar  on 2/11/2017.
  */

package DataAnalysis
import scala.math

//Main class for data generation

class dataGeneration(cont: Boolean = true, cumul: Boolean = false, dist: String, shape: Double, size : Int, loc: Double, scale: Double) {

  val distType = cont
  val cumulative = cumul
  val distPDF = dist
  val sampleSize = size
  val shp = shape
  val locParam = loc
  val scaleParam = scale

  // Main data structure holding the results from the data generation

  val dataGenResult = genDistData(distPDF, shp, size, locParam, scaleParam)

  //Gaussian distribution
  def genNorm(loc: Double, scale: Double, size: Int): Array[Double] = {

    def gaussian (x: Double) : Double = (1/scale*math.sqrt(2*math.Pi)) * math.exp(-math.pow(x - loc, 2) / (2*scale*scale))

    val independentVar :Array[Double] = Array.fill[Double](size)(math.random)
    return independentVar.map(x => gaussian(x))

  }

  //Weibull distribution
  def genWeibull(shp: Double, loc: Double, scale: Double, size: Int): Array[Double] = {
    def weibull (x: Double) : Double = (shp/scale) * math.pow( (x - loc) / scale, (shp - 1)) * math.exp( -math.pow( (x-loc)/scale, shp))

    val independentVar :Array[Double] = Array.fill[Double](size)(math.random)
    return independentVar.map(x => weibull(x))
  }
  //Lognormal distribution
  def genLogNorm(loc: Double, scale: Double, size: Int): Array[Double] = {
    def lognormal (x: Double) : Double = (1 / x*scale*math.sqrt(2*math.Pi)) * math.exp(- math.pow(math.log(x),2) / 2 * scale*scale)

    val independentVar :Array[Double] = Array.fill[Double](size)(math.random)
    return independentVar.map(x => lognormal(x))
  }

  //Main function that generates data from a distribution; uses pattern matching
  def genDistData(distType: String, shp: Double, size:Int, loc:Double, scale:Double ): Array[Double] = distType match {

    case "normal" => return genNorm(loc, scale, size)

    case "gaussian" => return genNorm(loc, scale, size)
      
    case "lognormal" => return genLogNorm(loc, scale, size)
      
    case "weibull" => return genWeibull(shp, loc, scale, size)
  }


}
