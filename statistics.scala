/**
  * Created by Rajesh Sampathkumar  on 2/11/2017.
  */

package DataAnalysis

object statistics {

  def main(args:Array[String]){

    //Returning categorical data stats
    val c = new catDataset(Array("Cat", "Dog", "Cat", "Dog", "Human", "Bird"))
    c.returnStats()

    val dataArray = new dataGen(true, false, "normal", 1, 100, 10, 1)
    val dataset = new varDataset(dataArray.dataGenResult)
    dataArray.dataGenResult.foreach(println)
    //dataset.returnStats()

  }

}
