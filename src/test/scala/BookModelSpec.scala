/**
  * Created by MacZ on 31/10/2016.
  */

import org.scalatest.{FlatSpec, Matchers}
import models._

import scala.collection.mutable.ListBuffer


class BookModelSpec extends FlatSpec with Matchers {

 "when entering strings of two books they" should "generate a list of 2 objects" in {
   val booksGenerated = List("Order of the Phoenix", "chamber of secrets")
   val basket:ListBuffer[book] = BookCreater.create(booksGenerated)
   basket.length should be (2)
}

}
