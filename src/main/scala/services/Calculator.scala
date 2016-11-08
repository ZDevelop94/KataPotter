package services

import models.book
import models.BookCreater

/**
  * Created by MacZ on 04/11/2016.
  */
trait Calculator {
  case class discountChecker (var count : Int, var discountApplied : Boolean)

  def total(basket: List[book]) : Double = {
    var finalPrice: Double = 0
    basket.foreach { book =>
    finalPrice = finalPrice + book.price
    }
    finalPrice
  }

  def discountCheck(basket: List[book]) : List[book] = {

   val phil, cham, pris, gob, ord = discountChecker(0, false)

    basket.foreach { oneBook => oneBook.name.toLowerCase match {
      case bookName if bookName == "philosopher's stone" => phil.count = phil.count + 1
      case bookName if bookName == "chamber of secrets" => cham.count  = cham.count  + 1
      case bookName if bookName == "prisoner of azkaban" => pris.count  = pris.count  + 1
      case bookName if bookName == "goblet of fire" => gob.count  = gob.count  + 1
      case bookName if bookName == "order of the phoenix" => ord.count  = ord.count + 1
      case _ => println("Not found")
    }}

    val discountCheckerList:List[discountChecker] = List(phil, cham, pris, gob, ord)

    applyDiscounts(totaldifferentbooks,discountCheckerList, basket)
    basket
  }

  def totaldifferentbooks(discountCheckerList : List[discountChecker]) : Int = {
    var totalDiff = 0
    discountCheckerList.foreach{ checker => if (checker.count >= 1) totalDiff = totalDiff + 1}
    totalDiff
  }

  def applyDiscounts(func : (List[discountChecker]) => Int, discountCheckerList : List[discountChecker], basket : List[book]) = {

    basket.foreach { book => book.name.toLowerCase match {
      case bookName if bookName == "philosopher's stone" => apply(discountCheckerList.head)
      case bookName if bookName == "chamber of secrets" => apply(discountCheckerList(1))
      case bookName if bookName == "prisoner of azkaban" => apply(discountCheckerList(2))
      case bookName if bookName == "goblet of fire" => apply(discountCheckerList(3))
      case bookName if bookName == "order of the phoenix" => apply(discountCheckerList(4))
    }

      def apply(discountChecker : discountChecker) = {
        discountChecker.discountApplied match {
          case false => func(discountCheckerList) match {
            case 2 => book.price = book.price - (book.price * 0.05)
              changeStatus(discountChecker)
            case 3 => book.price = book.price - (book.price * 0.10)
              changeStatus(discountChecker)
            case 4 => book.price = book.price - (book.price * 0.20)
              changeStatus(discountChecker)
            case x if x > 4 => book.price = book.price - (book.price * 0.25)
              changeStatus(discountChecker)
            case _ => book.price
          }
          case true => book.price
        }

        def changeStatus(discountChecker : discountChecker): Unit = {
          discountChecker.discountApplied = true
        }
      }
    }
  }
}
