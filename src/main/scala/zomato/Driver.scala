package zomato

import scala.collection.mutable.ListBuffer

object Driver {


  case class RestaurantDetails(id: Int,name: String, rating: Double, votes: Int,
                               location: String, restaurantType: List[String],likedDishes: List[String], cuisines: List[String],
                               cost: List[String])

  var parsed_data = ListBuffer[RestaurantDetails]()

  def parser() {
    val buffer = io.Source.fromFile("data/zomato_cleaned.csv")

    for (line <- buffer.getLines) {
      val cols = line.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)", -1).toList

      val parsedRow = RestaurantDetails(
        cols(0).toInt,
        cols(1),
        if(cols(2).contains('/'))cols(2).split('/').head.toDouble else 0.0,
        cols(3).toInt,
        cols(4),
        cols(5).split(',').toList,
        cols(6).split(',').toList,
        cols(7).split(',').toList,
        cols(8).split(',').toList
      )

      parsed_data += parsedRow
    }
    buffer.close()

    val N = 10
    print(top_restaurants_by_rating(N,parsed_data))


  }

  def top_restaurants_by_rating(N:Int,data:ListBuffer[RestaurantDetails]) = (data.sortBy(x => -x.rating).take(N))

  def top_restaurants_by_rating_location_type(N:Int,data:ListBuffer[RestaurantDetails],location:String,r_type:String) =
    (data.filter(x=>(x.location==location && x.restaurantType.contains(r_type))).sortBy(x => -x.rating).take(N))

  def top_restaurants_by_rating_location_votes(N:Int,data:ListBuffer[RestaurantDetails],location:String)=(
    data.filter(x=>(x.location==location)).sortBy(x=>(x.votes,-x.rating)).take(N)
  )

  def number_of_liked_dishes(data:ListBuffer[RestaurantDetails])=
    (data.map(x=>(x.id,x.likedDishes.length)))

  def distinct_locations(data:ListBuffer[RestaurantDetails])=
    (data.map(x=>x.location).distinct.length)

  def distinct_cuisines_by_location(data:ListBuffer[RestaurantDetails]) = ???
}
