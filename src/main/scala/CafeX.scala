
import Currency.{EUR, GBP, JPY, USD}

import java.time.{LocalDate, LocalTime}

object CafeX extends App {
  //THEME: FRENCH CAFE

  val cola = MenuItem("Cola", .50, isHot = false, isFood = false, isPremium = false)
  val coffee = MenuItem("Cafe", 1.00, isHot = true, isFood = false, isPremium = false)
  val redWine = MenuItem("Malbec", 4.00, isHot = false, isFood = false, isPremium = true)
  val whiteWine = MenuItem("Sauvignon Blanc", 4.00, isHot = false, isFood = false, isPremium = true)
  val beer = MenuItem("Kronenbourg", 3.00, isHot = false, isFood = false, isPremium = true)

  val cheeseSandwich = MenuItem("Cheese Sandwich", 2.00, isHot = false, isFood = true, isPremium = false)
  val steakFrites = MenuItem("Steak frites", 4.50, isHot = true, isFood = true, isPremium = false)
  val onionSoup = MenuItem("Onion Soup", 7.50, isHot = true, isFood = true, isPremium = false)
  val ratatouille = MenuItem("Ratatouille", 14.00, isHot = true, isFood = true, isPremium = false)
  val Quiche = MenuItem("Quiche Lorraine", 9.00, isHot = false, isFood = true, isPremium = false)

  val coqAuVin = MenuItem("Coq au vin", 18.50, isHot = true, isFood = true, isPremium = true)
  val lobster = MenuItem("Lobster", 25.00, isHot = true, isFood = true, isPremium = true)
  val caviar = MenuItem("Caviar", 30.00, isHot = false, isFood = true, isPremium = true)


  val connie = Customer("Connie", 4)
  val cristian = Customer("Cristian", 2)
  val sarina = Customer("Sarina", 3)
  val jake = Customer("Jake", 8)
  val robyn = Customer("Robyn", 6)
  val yonis = Customer("Yonis", 8)


  def currencyExchangeRate(currency: Currency): BigDecimal = {
    currency match {
      case GBP => 1
      case EUR => 1.18
      case USD => 1.24
      case JPY => 159.8
      case _ => 1
    }
  }

  def setCurrencySymbol(currency: Currency) = {
    currency match {
      case GBP => "£"
      case EUR => "€"
      case USD => "$"
      case _ => "£"
    }
  }

//  def IncreaseLoyaltyPoints(customer: Customer, items: List[MenuItem]): String = { //TODO: You've worked out the loyalty points and returned a messages, testing this could prove difficult. Next time return the points and construct the message elsewhere
//    if (billWithLoyaltyDiscount(customer, items) >= 20 && customer.loyaltyStars < 8) {
//      customer.loyaltyStars += 1 //TODO: This type of mutability strays away from what scala offers, try to do this with immutability
//      s"Loyalty point added! Current total: ${customer.loyaltyStars}"
//    } else if (billWithLoyaltyDiscount(customer, items) >= 20 && customer.loyaltyStars >= 8) {
//      "Maximum loyalty points reached! Congratulations you receive a 20% discount on all non-premium orders. "
//    }
//    else "Spend at least £20 next time to get a loyalty point"
//  }

  def loyaltyDiscount(customer: Customer, items: List[MenuItem]): BigDecimal =
    customer.loyaltyStars match {
      case (0) => 1 * sumFoodAndDrink(items)
      case (1) => 1 * sumFoodAndDrink(items)
      case (2) => 1 * sumFoodAndDrink(items)
      case (3) =>.025 * sumFoodAndDrink(items)
      case (4) =>.05 * sumFoodAndDrink(items)
      case (5) =>.075 * sumFoodAndDrink(items)
      case (6) =>.1 * sumFoodAndDrink(items)
      case (7) =>.125 * sumFoodAndDrink(items)
      case (8) =>.15 * sumFoodAndDrink(items)
    }

  private def sumMenuItems(menuItems: List[MenuItem]): BigDecimal = menuItems.map(item => item.cost).sum

  def sumFoodAndDrink(items: List[MenuItem]): BigDecimal = happyHourHalfPriceDrinks(items) + getFoodItems(items).map(items => items.cost).sum

  def billWithLoyaltyDiscount(customer: Customer, items: List[MenuItem]) = {
    if (checkIfOrderContainsPremiumFood(items)) { //TODO: premiumFood isn't very descriptive, I know this is picky but "isPremiumFood" `sounds` nicer
      sumFoodAndDrink(items)
    }
    else {
      sumFoodAndDrink(items) - loyaltyDiscount(customer, items)
    }
  }

  def displayLoyaltyDiscount(customer: Customer, items: List[MenuItem]): BigDecimal = {
    if (checkIfOrderContainsPremiumFood(items)) {
      0
    } else loyaltyDiscount(customer, items)
  }

  def displayHappyHourDiscount(items: List[MenuItem]): BigDecimal = {
    if(checkIfIsHappyHour()){
      happyHourHalfPriceDrinks(items)
    } else 0
  }

  def getDrinkItems(items: List[MenuItem]): List[MenuItem] = items.filter(items => !items.isFood)

  def getFoodItems(items: List[MenuItem]): List[MenuItem] = items.filter(items => items.isFood)

  def checkIfOrderIsOnlyDrinks(items: List[MenuItem]) = !items.exists(items => items.isFood)

  def checkIfOrderContainsHotFood(items: List[MenuItem]): Boolean = items.exists(item => item.isHot && item.isFood)

  def checkIfOrderContainsPremiumFood(items: List[MenuItem]): Boolean = items.exists(item => item.isPremium)

  def checkIfOrderContainsDrinks(items: List[MenuItem]): Boolean = items.exists(items => !items.isFood)

  def checkIfIsHappyHour(): Boolean = {
    val time = LocalTime.now().getHour
    (time > 10 && time < 21)}

  def serviceChargeCalculator(items: List[MenuItem]): BigDecimal = {
    if (checkIfOrderIsOnlyDrinks(items)) {
      0
    } else if (!checkIfOrderIsOnlyDrinks(items) && !checkIfOrderContainsHotFood(items) && !checkIfOrderContainsPremiumFood(items)) {
      sumMenuItems(items) * 0.1
    } else if (checkIfOrderContainsHotFood(items) && !checkIfOrderContainsPremiumFood(items)) {
      sumMenuItems(items) * 0.2
    } else if (checkIfOrderContainsPremiumFood(items)) {
      sumMenuItems(items) * 0.25
    } else 0
  }

  def serviceChargeCap(items: List[MenuItem]): BigDecimal = {
    if (serviceChargeCalculator(items) <= 20) {
      serviceChargeCalculator(items)
    } else if (serviceChargeCalculator(items) >= 20 && !items.exists(items => items.isPremium)) {
      20
    } else if (serviceChargeCalculator(items) > 40 && items.exists(items => items.isPremium)) {
      40
    } else serviceChargeCalculator(items)
  }

  def happyHourHalfPriceDrinks(items: List[MenuItem]): BigDecimal = {
    val drinksCost = items.filter(items => !items.isFood).map(drinks => drinks.cost)
    if (checkIfIsHappyHour()) {
      (drinksCost.sum) / 2
    } else drinksCost.sum
  }

  def billTotalWithServiceAndLoyalty(customer: Customer, items: List[MenuItem]): BigDecimal = {
    val billWithLoyalty = billWithLoyaltyDiscount(customer, items)
    val serviceCharge = serviceChargeCalculator(items)
    val total = billWithLoyalty + serviceCharge
    total
  }

  def billReceipt(customer: Customer, items: List[MenuItem], currency: Currency): String = {
    val symbol = setCurrencySymbol(currency)
    "Thank you for ordering at X Cafe! \n" +
      s"Date of transaction:  ${LocalDate.now()}" + "\n" +
      s"Time of transaction:  ${LocalTime.now().getHour}:${LocalTime.now().getMinute}" + "\n" +
      "----------------------------------------------------- \n" +
//      IncreaseLoyaltyPoints(customer, items) +
      "Your Order: \n" + items.map(food => (food.item)) + "\n" +
      "-----------------------------------------------------" +
      "\n" +
      s"Order before discount and service: $symbol${sumMenuItems(items) * currencyExchangeRate(currency)}" +
      "\n" +
      s"Loyalty Discount: - $symbol${displayLoyaltyDiscount(customer, items) * currencyExchangeRate(currency)}" +
      "\n" +
      s"Happy Hour Drinks Discount: - $symbol${displayHappyHourDiscount(items) * currencyExchangeRate(currency)}" +
      "\n" +
      s"Service Charge: + $symbol${serviceChargeCap(items) * currencyExchangeRate(currency)}" +
      "\n" +
      "\n" + s"Total: $symbol${billTotalWithServiceAndLoyalty(customer, items) * currencyExchangeRate(currency)}"
  }











val order = List(steakFrites, lobster, cola, cheeseSandwich, whiteWine)
//
//  println(billReceipt(cristian, order, USD))
//  println("-----------------------------------------------------")

  println(billReceipt(cristian, order, GBP))
  println("-----------------------------------------------------")

//  println(billReceipt(cristian, order, JPY))
//  println("-----------------------------------------------------")

//  println(billReceipt(jake, List(steakFrites, cola, cheeseSandwich, Ratatouille)))
//  println("-----------------------------------------------------")
// println(happyHourHalfPriceDrinks(List(steakFrites, cola, cheeseSandwich, Ratatouille)))
//  println("-----------------------------------------------------")


//  println("loyalty discount:")
//  println(loyaltyDiscount(jake,List(steakFrites, cola, cheeseSandwich, Ratatouille) ))
//
//  println("sum of food and drink:")
//  println(sumFoodAndDrink(List(steakFrites, cola, cheeseSandwich, Ratatouille)))
//  println("happy hour drinks discount:")
//  println(happyHourHalfPriceDrinks(List(steakFrites, cola, cheeseSandwich, Ratatouille)))
//  println("sum of menu items:")
//  println(sumMenuItems(List(steakFrites, cola, cheeseSandwich, Ratatouille)))
//
//  println("-----------------------------------------------------")
//  println("bill with loyalty discount:")
//  println(billWithLoyaltyDiscount(jake,List(steakFrites, cola, cheeseSandwich, Ratatouille)))

}
