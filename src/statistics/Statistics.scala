package statistics

object Statistics {

  def average[T](data: List[T], f: T => Double): Double = {
    val functionApplied : List[Double] = for(item <- data) yield{
      f(item)
      }
    val sum : (Double, Double) => Double = (acc : Double, b : Double) => acc + b
    val sumOfData : Double = functionApplied.reduce(sum)
    sumOfData/data.length
  }

  def topK[T] (data : List[T], f: T => Double, k : Int) : List[T] = {
    def compareData (a1 : T, a2 : T): Boolean = {
      f(a1) > f(a2)
    }
    if (data.length < k) {
      data.sortWith(compareData)
    }
    else {
      val sortedData = data.sortWith(compareData)
      sortedData.dropRight(k-1)
    }
  }

  def bayesianAverage[T] (data : List[T], f: T => Double, extraRatings : Int, valExtraRatings : Int) : Double = {
    val functionApplied : List[Double] = for(item <- data) yield{
      f(item)
    }
    val extraRatingsAdded : List[Double] = (for(i <- 1 to extraRatings) yield {
      valExtraRatings.toDouble
    }).toList
    val combinedList : List[Double] = functionApplied ::: extraRatingsAdded
    val sum : (Double, Double) => Double = (acc : Double, b : Double) => acc + b
    val sumOfData : Double = combinedList.reduce(sum)
    sumOfData/combinedList.length
  }



}
