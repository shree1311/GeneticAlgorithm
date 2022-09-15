package music

import music.Music._

import scala.language.postfixOps
import scala.io.Source

object Music {


  // TODO: Implement required methods as defined in the HW Doc


  /**
   * You may use this helper method to read a file and return a List of Strings containing the lines of the file
   *
   * @param filename The file to be read
   * @return The lines of the file as a List of Strings with 1 String per line
   */
  def filenameToListOfLines(filename: String): List[String] = {
    val file = Source.fromFile(filename)
    val lines: List[String] = file.getLines().toList
    file.close()
    lines
  }

  // TODO: Uncomment after implementing the required methods
//   Uses your methods to read all the data in a file with song ratings
    def readSongsFromFile(filename: String): List[Song] = {
      val songsWithDuplicates: List[Song] = readSongsFromFileWithoutDuplicates(filename)
      val songMap: Map[String, Song] = songListToMap(songsWithDuplicates)
      songMap.values.toList
    }

  // Can be used to test your application objective
  def songIncubator(songs: List[Song]): List[Double] => Song = {
    genes: List[Double] => {
      val geneSong: Int = (genes.head.abs * songs.length).toInt % songs.length
      songs(geneSong)
    }
  }

  // Can be used to test your application objective
  def playlistIncubator(songs: List[Song]): List[Double] => Playlist = {
    genes: List[Double] => {
      val incubatorForSongs: List[Double] => Song = songIncubator(songs)
      new Playlist(genes.map((gene: Double) => incubatorForSongs(List(gene))))
    }
  }

  def songCostFunction(map: Map[String, Int]): Song => Double = {
    (song: Song) => {
      val rating = map.getOrElse(song.youtubeId, 3)
      if (rating < 3) {
        1000.0
      }
      else {
        1 / (song.bayesianRating(2, 3) * rating)
      }
    }
  }

  def readUserRatingsFromFile(filename: String): Map[String, Int] = {
    val fileAsList: List[String] = filenameToListOfLines(filename)
    splitList(fileAsList)
  }

  def splitList(inputData: List[String]): Map[String, Int] = {
    if (inputData.length < 2) {
      val func: (String, String) => String = (acc: String, b: String) => acc + b
      val data = inputData.reduce(func)
      val splits = data.split(",")
      val map: Map[String, Int] = Map(splits(0) -> splits(3).toInt)
      map
    }
    else {
      val mid: Int = inputData.length / 2
      val (left, right) = inputData.splitAt(mid)
      val leftSorted = splitList(left)
      val rightSorted = splitList(right)
      merge(leftSorted, rightSorted)
    }
  }

  def merge(map1: Map[String, Int], map2: Map[String, Int]): Map[String, Int] = {
    map1 ++ map2
  }

  def readSongsFromFileWithoutDuplicates(filename: String): List[Song] = {
    val fileAsList: List[String] = filenameToListOfLines(filename)
    createListOfSongs(fileAsList)
  }

  def createListOfSongs(inputData: List[String]): List[Song] = {
    if (inputData.length < 2) {
      val func: (String, String) => String = (acc: String, b: String) => acc + b
      val data = inputData.reduce(func)
      val splits = data.split(",")
      val list: List[Song] = List(new Song(splits(2), splits(1), splits(0), List(new SongRating(splits(3).toInt, splits(4).toInt))))
      list
    }
    else {
      val mid: Int = inputData.length / 2
      val (left, right) = inputData.splitAt(mid)
      val leftSorted = createListOfSongs(left)
      val rightSorted = createListOfSongs(right)

      mergeList(leftSorted, rightSorted)
    }
  }

  def mergeList(list1: List[Song], list2: List[Song]): List[Song] = {
    list1 ::: list2
  }

  def songListToMap(listOfSongs: List[Song]): Map[String, Song] = {
    val map = makeMap(listOfSongs,Map())
    map
  }


  def makeMap(listofSongs: List[Song], map : Map[String,Song]): Map[String, Song] = {
    if (listofSongs.isEmpty){
      map
    }
    else {
      if(map.contains(listofSongs.head.youtubeId)){
        val newMap : Map[String, Song] = map + (listofSongs.head.youtubeId -> listofSongs.head.addMultipleRatings(map.getOrElse(listofSongs.head.youtubeId,song).ratings))
        makeMap(listofSongs.drop(1),newMap)
      }
      else{
        val newMap : Map[String, Song] = map + (listofSongs.head.youtubeId -> listofSongs.head)
        makeMap(listofSongs.drop(1), newMap)
      }
    }
  }

  val song: Song = new Song("","","",List(new SongRating(0,0)))

}








