/**
  * Created by Rajesh Sampathkumar on 2/11/2017.
  */

package DataAnalysis


class varDataset (array: Array[Double]) {

  val data = array

  def returnStats() = {

    var mean = data.reduce( (x,y) => x+y )/data.length
    var sd = data.map(x => Math.pow(x - mean,2)).reduce((x,y) => x+y)/(data.length -1)
    var zScores = data.map( x => (x - mean) / sd )
    var skewness = data.map( x => Math.pow( x-mean, 3)).reduce( (x,y) => x+y )/(data.length * Math.pow(sd,3))
    var kurtosis = data.map( x => Math.pow( x-mean, 4)).reduce( (x,y) => x+y )/(data.length * Math.pow(sd,4))

    println(mean, sd, skewness, kurtosis)
    println("Z scores of points: ")
    zScores.foreach(println)

  }
}
