package DataAnalysis

/**
  * Created by Rajesh Sampathkumar on 2/11/2017.
  */


class catDataset (column: Array[String]) {

  val data = column

  def returnStats() = {


  def reduceByKey[K,V](collection: Traversable[Tuple2[K, V]])(implicit num: Numeric[V]): Map[K, V] = {
    import num._
    collection
      .groupBy(_._1)
      .map { case (group: K, traversable) => traversable.reduce{(a,b) => (a._1, a._2 + b._2)} }
  }

    val catFreq = reduceByKey(data.map(x => (x,1)))

    println("Categories: \n")
    println( catFreq.map(x => x._1).toList.foreach(println) )

    println("\nFrequencies: \n")
    println( catFreq.map(x => x._2).toList.foreach(println) )
  }

}
