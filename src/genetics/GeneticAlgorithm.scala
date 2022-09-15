package genetics

import com.mysql.cj.xdevapi.DatabaseObject

import scala.util.Random

object GeneticAlgorithm {

  def geneticAlgorithm[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int): T = {

    def populate(howManyLists: Int, howManyGenes: Int): List[List[Double]] = {
      List.fill(howManyLists)(List.fill(howManyGenes)(Random.between(-100.0, 100.0)))
    }

    def sort(list: List[List[Double]]): List[List[Double]] = {
      def comparator(a1: List[Double], a2: List[Double]): Boolean = {
        costFunction(incubator(a1)) < costFunction(incubator(a2))
      }
      list.sortWith(comparator)
    }

    def survival(list: List[List[Double]], splitLocation: Int): List[List[Double]] = {
      list.dropRight(splitLocation - 1)
    }

    def mutate(list: List[List[Double]], howManyMutation: Int, numberToMutate: Int): List[List[Double]] = {
      val bestSolution: List[Double] = list(0)
      val updatedList: List[List[Double]] = list.drop(0)
      val mutatedList: List[List[Double]] = for (list <- updatedList) yield {
        for (double <- list) yield {
          double + Random.between(-0.015, 0.016)
        }
      }
      val (left, right) = updatedList.splitAt(numberToMutate)
      val List: List[List[Double]] = left ::: mutatedList.dropRight(numberToMutate - 1)
      bestSolution :: List
    }

    def crossover (parent1 : List[Double], parent2 : List[Double]) : List[Double] = {
      val child : List[Double] = (for(index <- 0 to parent1.length-1) yield {
        (parent1(index) + parent2(index))/2
      }).toList
      child
    }

    def moreAnimals (howManyAnimals : Int, currentAnimals : List[List[Double]]) : List[List[Double]]={
      val currentLength = currentAnimals.length
      val newAnimals = populate(7-currentLength,numberOfGenes)
      val output = currentAnimals ::: newAnimals
      output
    }

    val animals = populate(7,numberOfGenes)
    val sorted = sort(animals)
    val fittest = survival(sorted,5)
    val mutation = mutate(fittest,10,3)
    val crossover1 = crossover(mutation(1),mutation(2))
    val crossover2 = crossover(mutation(3),mutation(4))
    val crossover3 = crossover(mutation(5),mutation(6))
    val crossed : List[List[Double]]= List(crossover1 ::: crossover2 ::: crossover3)
    val newGen = moreAnimals(7,crossed)
    val output = incubator(sort(newGen)(0))
    output
  }

}

