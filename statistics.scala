/**
  * Created by Rajesh Sampathkumar  on 2/11/2017.
  */

package DataAnalysis

object statistics {

  def main(args:Array[String]){

    //Returning categorical data stats
    val c = new categoricalUnivariate(Array("Cat", "Dog", "Cat", "Dog", "Human", "Bird"))
    c.returnStats()


    //Generating data for continuous data statistics
    val dataArray = new dataGeneration(true, false, "lognormal", 1, 100, 10, 1)
    val dataset = new continuousUnivariate(dataArray.dataGenResult)

    println("Data array generated below: ")
    dataArray.dataGenResult.foreach(println)

    println("Statistics on data array below:")
    dataset.returnStats()

    println("Demonstrating correlation coefficient between two sets of data.")
    val dag1 = new dataGeneration(true, false, "gaussian", 1, 10, 10, 1)
    val dag2 = new dataGeneration(true, false, "gaussian", 1, 10, 10, 1)

    val da1 = dag1.dataGenResult
    val da2 = dag1.dataGenResult

    println("Data set 1: ")
    da1.foreach(println)

    println("\nData set 2:")
    da2.foreach(println)

    //Testing the Pearson R on a dataset and its derivative
    val pearsonR = new continuousBivariate(da1, da1.map(x => 3*x + math.random))
    println("Pearson R " + pearsonR.corrCoef().toString)

  }

}
