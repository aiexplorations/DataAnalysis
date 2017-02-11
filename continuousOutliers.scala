/**
  * Created by Rajesh Sampathkumar on 2/11/2017.
  */
package DataAnalysis
import scala.math

//Finds outliers beyond a specified z-score in a data set
class continuousOutliers(data: Array[Double], targetZ: Double) {
  val xVals = data
  val zMax = targetZ

  def zScore (value:Double, mean: Double, sd:Double): Double = { (value - mean) / sd }

  val xMean = xVals.reduce((x,y) => x+y )/xVals.length
  val xSD = math.sqrt(xVals.map(x => math.pow(x - xMean,2)).reduce((x,y) => x+y)/(xVals.length -1))

  def isOutlier(xval: Double, mean: Double, sd: Double ): Int = {
    if(zScore(xval, mean, sd) > zMax) 1
    else 0
  }

  val outlierFlag = xVals.map( x => isOutlier(x, xMean, xSD))

  def getOutliers = outlierFlag
  def countOutliers = outlierFlag.reduce( (x,y) => x+y)

  def getZScores = xVals.map( x => zScore(x, xMean, xSD))
}
