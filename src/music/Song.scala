package music

import statistics.Statistics

class Song(val title: String, val artist: String, val youtubeId: String, val ratings: List[SongRating]) {

  def averageRating(): Double = {
    // This is an example of calling your average function to get the average rating of a song
    Statistics.average(this.ratings, (rating: SongRating) => rating.rating)
  }

  def averageEnergyRating(): Double = {
    // This is an example of calling your average function to get the average energy rating of a song
    Statistics.average(this.ratings, (rating: SongRating) => rating.energyLevel)
  }

  // TODO: Uncomment this method after writing Statistics.bayesianAverage
//   Compute the bayesian average of song ratings
  def bayesianRating(extraRatings: Int, valueOfExtraRatings: Int): Double = {
    Statistics.bayesianAverage(this.ratings, (rating: SongRating) => rating.rating, extraRatings, valueOfExtraRatings)
  }

  def addRating(rating: SongRating) : Song ={
    new Song (this.title,this.artist,this.youtubeId,rating::this.ratings)
  }

  def addMultipleRatings(ratings : List[SongRating]) : Song = {
    new Song(this.title,this.artist,this.youtubeId,ratings:::this.ratings)
  }

}
