/**
  * Created by Rajesh Sampathkumar on 2/11/2017.
  */

package DataAnalysis

class continuousUnivariate(array: Array[Double]) {

  val data = array

  def returnStats() = {

    val mean = data.reduce( (x,y) => x+y )/data.length
    val sd = data.map(x => Math.pow(x - mean,2)).reduce((x,y) => x+y)/(data.length -1)
    val zScores = data.map( x => (x - mean) / sd )
    val skewness = data.map( x => Math.pow( x-mean, 3)).reduce( (x,y) => x+y )/(data.length * Math.pow(sd,3))
    val kurtosis = data.map( x => Math.pow( x-mean, 4)).reduce( (x,y) => x+y )/(data.length * Math.pow(sd,4))

    println ("\nMean: "+mean.toString,"\nStandard deviation: "+sd.toString,"\nSkewness: "+skewness.toString,"\nKurtosis: "+kurtosis.toString)
    println("Z scores of points: ")
    zScores.foreach(println)
  }
}
