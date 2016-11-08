/**
  * Created by MacZ on 31/10/2016.
  */
import org.scalatest.{FlatSpec, Matchers, WordSpec}
import models._
import services._

class PricingModelSpec extends WordSpec with Matchers with Calculator{


  "Order of the Phoenix" should {
    "be 8 EUR" in {
      val orderOfPhoenix = book(1, "Order of the Phoenix", 8.00)
      val basket = List(orderOfPhoenix)
      basket.head.price should be (8.00)
    }
  }

  "If two different books are purchased the software" should {
    val orderOfPhoenix = book(1, "Order of the Phoenix", 8.00)
    val chamberOfSecrets = book(2, "Chamber of secrets", 8.00)
    val basket = List(orderOfPhoenix, chamberOfSecrets)

    "know they are two different books" in {

      val basketChecked = discountCheck(basket)

      basketChecked.head.price should be(7.6)
      basketChecked(1).price should be(7.6)
      //SoldBooks.map { x => Calculator.duplicates}

    }

    "apply a discount of 5%" in {
      total(basket) should be(15.2)
    }

  }

  "If three different books are purchased the software" should {
    val orderOfPhoenix = book(1, "Order of the Phoenix", 8.00)
    val chamberOfSecrets = book(2, "Chamber of secrets", 8.00)
    val gobletOfFire = book(3, "goblet of fire", 8.00)
    val basket = List(orderOfPhoenix, chamberOfSecrets, gobletOfFire)

    "know they are three different books" in {

      val basketChecked = discountCheck(basket)

      basketChecked.head.price should be(7.2)
      basketChecked(1).price should be(7.2)
      //SoldBooks.map { x => Calculator.duplicates}

    }

    "apply a discount of 10%" in {
      total(basket) should be(21.6)
    }

  }

  "If two of the same books are purchased the software" should {

      val orderOfPhoenix = book(1, "Order of the Phoenix", 8.00)
      val orderOfPhoenix2 = book(2, "Order of the Phoenix", 8.00)
      val basket = List(orderOfPhoenix, orderOfPhoenix2)

      "know they are the same book" in {

        val basketChecked = discountCheck(basket)

        basketChecked.head.price should be(8.00)
        basketChecked(1).price should be(8.00)
      }

      "apply no discount" in {
        total(basket) should be(16.00)
      }
  }

  "If there are three duplicates and three different books purchased the software" should {
      ////Finish
      val orderOfPhoenix = book(1, "Order of the phoenix", 8.00)
      val orderOfPhoenix2 = book(2, "Order of the phoenix", 8.00)
      val orderOfPhoenix3 = book(3, "Order of the phoenix", 8.00)
      val chamberOfSecrets = book(4, "Chamber of secrets", 8.00)
      val philosophers = book(5, "Philosopher's stone", 8.00)

      val basket = List(
        orderOfPhoenix,
        orderOfPhoenix2,
        orderOfPhoenix3,
        chamberOfSecrets,
        philosophers
      )

        "know they are the same book" in {

          val basketChecked:List[book] = discountCheck(basket)

          basketChecked.map{
            case x if x.id == 1 =>  x.price should be (7.20)
            case x if x.id == 2 || x.id == 3  =>  x.price should be (8.00)
            case x => x.price should be (7.20)
          }
        }

      "apply one discount of 10%" in {
          total(basket) should be (37.60)
        }
      }

}

