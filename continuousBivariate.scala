/**
  * Created by Rajesh Sampathkumar on 2/11/2017.
  */

package DataAnalysis
import scala.math


class continuousBivariate(one: Array[Double], two: Array[Double] ) {

  val x = one
  val y = two
  val pearsonR = corrCoef()

  def corrCoef(): Double ={

    if (x.length == y.length){

      /* Correlation coefficient can be calculated based on linear combinations of the Z scores of individual points
       * More here: https://en.wikipedia.org/wiki/Pearson_correlation_coefficient
       */

      def zScore (value:Double, mean: Double, sd:Double): Double = { (value - mean) / sd }

      val xMean = x.reduce((x,y) => x+y )/x.length
      val yMean = y.reduce((x,y) => x+y )/y.length
      val xSD = math.sqrt(x.map(x => Math.pow(x - xMean,2)).reduce((x,y) => x+y)/(x.length -1))
      val ySD = math.sqrt(y.map(y => Math.pow(y - yMean,2)).reduce((x,y) => x+y)/(y.length -1))

      val zX = x.map(xval => zScore(xval, xMean, xSD ))
      val zY = y.map(yval => zScore(yval, yMean, ySD))
      val zProducts = zX.zip(zY).map(x => x._1 * x._2).reduce((x,y) => x+y)

      /*
      println("zProducts: "+zProducts.toString)
       */
      zProducts/ (x.length -1)
    }
    else{
      println ("Correlation: Data sets should be of equal length")
      0.0
    }

  }


}
