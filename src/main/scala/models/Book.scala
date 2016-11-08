package models

import scala.collection.mutable.ListBuffer

/**
  * Created by MacZ on 04/11/2016.
  */
case class book(id: Int, name: String, var price: Double)

object BookCreater {

  def create(createList: List[String]) = {

    val newBooks:ListBuffer[book] = ListBuffer.empty

     createList.map {

       case x : String => x.toLowerCase match {
        case "philosopher's stone" => newBooks += book(emptyListCheck(newBooks), "Philosopher's stone", 8)
        case "chamber of secrets" => newBooks  += book(emptyListCheck(newBooks), "Chamber of secrets", 8)
        case "prisoner of azkaban" => newBooks += book(emptyListCheck(newBooks), "Prisoner of Azkaban", 8)
        case "goblet of fire" => newBooks += book(emptyListCheck(newBooks), "Goblet of fire", 8)
        case "order of the phoenix" => newBooks += book(emptyListCheck(newBooks), "Order of Phoenix", 8)
      }
      case _ => createList
    }

    def emptyListCheck(ListOfBooks: ListBuffer[book]) = if (ListOfBooks.nonEmpty) {
      ListOfBooks.last.id + 1
    } else 1

    newBooks
  }//




}





