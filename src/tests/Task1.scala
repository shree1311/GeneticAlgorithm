package tests

import music.{Song, SongRating,Music}
import org.scalatest._
import statistics.Statistics

class Task1 extends FunSuite {


  test("test average") {
    val rating1 = new SongRating(5,3)
    val rating2 = new SongRating(5,3)
    val song:Song = new Song("Better","Khalid", "/khalidBetter", List(rating1,rating2))
    assert(Math.abs(song.averageRating()-5.0)<0.001)
  }

  test ("add bayesian average"){
    val rating1 = new SongRating(5,3)
    val rating2 = new SongRating(5,3)
    val song:Song = new Song("Better","Khalid", "/khalidBetter", List(rating1,rating2))
    assert(Math.abs(song.averageRating()-5.0)<0.001)
    assert(Math.abs(song.bayesianRating(2,3)-4.0)<0.001)
  }

  test ("topK"){
    val list : List[Int] = List(2,1,3,4,3)
    def func (input : Int) : Double ={
      input.toDouble
    }
    val retList = Statistics.topK(list,func, 3)
    assert(retList == List(4,3,3))
  }

  test("topK2"){
    val list : List[Int] = List(2,1,3,4,3)
    def func (input : Int) : Double ={
      input.toDouble
    }
    val retList = Statistics.topK(list,func, 7)
    assert(retList == List(4,3,3,2,1))
  }

  test ("song cost - no rating"){
    val rating1 = new SongRating(5,3)
    val rating2 = new SongRating(5,3)
    val song:Song = new Song("Better","Khalid", "/khalidBet", List(rating1,rating2))
    assert(Math.abs(Music.songCostFunction(Map("/khalidBetter" -> 2))(song)-0.083)<0.001)
  }

  test ("song cost - rating 1"){
    val rating1 = new SongRating(5,3)
    val rating2 = new SongRating(5,3)
    val song:Song = new Song("Better","Khalid", "/khalidBetter", List(rating1,rating2))
    assert(Math.abs(Music.songCostFunction(Map("/khalidBetter" -> 1))(song)-1000.0)<0.001)
  }

    test ("song cost - rating 2"){
      val rating1 = new SongRating(5,3)
      val rating2 = new SongRating(5,3)
      val song:Song = new Song("Better","Khalid", "/khalidBetter", List(rating1,rating2))
      assert(Math.abs(Music.songCostFunction(Map("/khalidBetter" -> 2))(song)-1000.0)<0.001)
    }

  test ("song cost - rating 3"){
    val rating1 = new SongRating(2,3)
    val rating2 = new SongRating(5,3)
    val song:Song = new Song("Better","Khalid", "/khalidBetter", List(rating1,rating2))
    assert(Math.abs(Music.songCostFunction(Map("/khalidBetter" -> 3))(song)-0.102564)<0.001)
  }

  test ("song cost - rating 4"){
    val rating1 = new SongRating(2,3)
    val rating2 = new SongRating(5,3)
    val rating3 = new SongRating(4,3)
    val song:Song = new Song("Better","Khalid", "/khalidBetter", List(rating1,rating2,rating3))
    assert(Math.abs(Music.songCostFunction(Map("/khalidBetter" -> 4))(song)-0.0735294)<0.001)
  }

  test ("song cost - rating 5"){
    val rating1 = new SongRating(2,3)
    val rating2 = new SongRating(5,3)
    val song:Song = new Song("Better","Khalid", "/khalidBetter", List(rating1,rating2))
    assert(Math.abs(Music.songCostFunction(Map("/khalidBetter" -> 5))(song)-0.0615384)<0.001)
  }

}
