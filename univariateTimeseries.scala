/**
  * Created by Rajesh Sampathkumar on 2/12/2017.
  */

package DataAnalysis

import scala.collection.mutable.ArrayBuffer

class univariateTimeseries (series: Array[Double], lags : Int) {

  val data = series
  val lag = lags



  // Implements the autocorrelation function as described in NIST
  // Reference: http://www.itl.nist.gov/div898/handbook/eda/section3/eda35c.htm

  def acorrFunc = {

    val xMean = data.reduce( (x,y) => x+y)/data.length

    var laggedData = new ArrayBuffer[Double]
    for (i <- lag to data.length-1){
      laggedData.append(data(i))
    }

    println("\n\n")
    //Component of numerator based on original data set
    val numRK1 = data.map( x => x - xMean)

    //Component of numerator based on lagged data set
    val numRK2 = laggedData.map(x => x - xMean)

    //Numerator and denominator calculations
    val numRK = numRK1.zip(numRK2).map(x => x._1 * x._2).reduce((x,y) => x+y)
    val denomRK = data.map(x => (x - xMean)*(x - xMean)).reduce((x,y) => x + y)

    //Displaying the autocorrelation coefficient
    print ("Autocorrelation coefficient is: "+(numRK/denomRK).toString)
  }

}
