package tests

import music.{Song, SongRating, Music}
import org.scalatest._
import statistics.Statistics

class Task3 extends FunSuite {

  test("add one rating") {
    val rating1 = new SongRating(5, 3)
    val rating2 = new SongRating(5, 3)
    val song: Song = new Song("Better", "Khalid", "/khalidBetter", List(rating2))
    song.addRating(rating1)

    val expectedSong: Song = new Song("Better", "Khalid", "/khalidBetter", List(rating2, rating1))
  }

  test("add multiple ratings") {
    val rating1 = new SongRating(5, 3)
    val rating2 = new SongRating(5, 3)
    val song: Song = new Song("Better", "Khalid", "/khalidBetter", List(rating1, rating2))
    val ratings: List[SongRating] = List(rating1, rating2)
    song.addMultipleRatings(ratings)

    val expectedSong: Song = new Song("Better", "Khalid", "/khalidBetter", List(rating2, rating1, rating2, rating1))
  }

  test("handle duplicates") {
    val rating1 = new SongRating(3, 3)
    val rating2 = new SongRating(2, 3)
    val song: Song = new Song("Better", "Khalid", "/khalidBetter", List(rating2, rating1))
    val song2: Song = new Song("Better", "Khalid", "/khalidBetter", List(rating2, rating1))
    val listOfSongs: List[Song] = List(song, song2)
    val mapOfSongs: Map[String, Song] = Music.songListToMap(listOfSongs)
    val expected: Map[String, Song] = Map("/khalidBetter" -> new Song("Better", "Khalid", "/khalidBetter", List(rating2, rating1,rating2,rating1)))
    compareSongMaps(mapOfSongs,expected)
  }

  def compareSongs(expected: Song, computed: Song): Unit = {
    assert(expected.youtubeId == computed.youtubeId)
    assert(expected.title == computed.title)
    assert(expected.artist == computed.artist)
    assert(expected.ratings.length == computed.ratings.length)
    val expectedRatings: List[SongRating] = expected.ratings
    val computedRatings: List[SongRating] = computed.ratings

    def orderRatings(a1: SongRating, a2: SongRating): Boolean = {
      a1.rating < a2.rating
    }

    val sortedExpected: List[SongRating] = expectedRatings.sortWith(orderRatings)
    val sortedComputed: List[SongRating] = computedRatings.sortWith(orderRatings)
    for (i <- 1 to sortedComputed.length) {
      assert(sortedExpected(i).rating == sortedComputed(i).rating)
      assert(sortedExpected(i).energyLevel == sortedComputed(i).energyLevel)
    }
  }
  //USE FOR ADD SONG RATING
  //need to use custom comparator to sort the ratings. Order of lists needs to be same so write a sorting function.
  //can use for loop to go through the list and make sure values of both rating and energy level is the same

  def compareSongMaps(actual: Map[String, Song], expected: Map[String, Song]): Unit = {
    assert(expected.size == actual.size)
    val actualKeys = actual.keySet.toList
    val expectedKeys = expected.keySet.toList
    assert(actualKeys==expectedKeys)

    for (key<-actualKeys) {
      val actualSong = actual.getOrElse(key, new Song("", "", "", List(new SongRating(0, 0))))
      val expectedSong = expected.getOrElse(key, new Song("a", "", "", List(new SongRating(0, 0))))
      assert(actualSong.title == expectedSong.title)
      assert(actualSong.artist == expectedSong.artist)
      assert(actualSong.youtubeId == expectedSong.youtubeId)
      assert(actualSong.ratings.length == expectedSong.ratings.length)
      val expectedRatings: List[SongRating] = expectedSong.ratings
      val computedRatings: List[SongRating] = actualSong.ratings
    }
    def orderRatings(a1: SongRating, a2: SongRating): Boolean = {
      a1.rating < a2.rating
    }
  }


  //TODO: Check if each map contains the same keys
  // TODO : Check if each key maps to equivalent Songs in each Map.


}
