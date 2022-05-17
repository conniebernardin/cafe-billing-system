import java.time
import java.time.LocalTime

object cafeX extends App {
  //MENU
  //THEME: FRENCH CAFE


  //INSTANTIATING ITEMS
  val cola = menu("Cola", .50, isHot = false, isFood = false, isPremium = false, isAlcoholic = false)
  val coffee = menu("Cafe", 1.00, isHot = true, isFood = false, isPremium = false, isAlcoholic = false)
  val redWine = menu("Malbec", 4.00, isHot = false, isFood = false, isPremium = true, isAlcoholic = true)
  val whiteWine = menu("Sauvignon Blanc", 4.00, isHot = false, isFood = false, isPremium = true, isAlcoholic = true)
  val beer = menu("Kronenbourg", 3.00, isHot = false, isFood = false, isPremium = true, isAlcoholic = true)

  val cheeseSandwich = menu("Cheese Sandwich", 2.00, isHot = false, isFood = true, isPremium = false, isAlcoholic = false)
  val steakFrites = menu("Steak frites", 4.50, isHot = true, isFood = true, isPremium = false, isAlcoholic = false)
  val onionSoup = menu("Onion Soup", 7.50, isHot = true, isFood = true, isPremium = false, isAlcoholic = false)
  val ratatouille = menu("Ratatouille", 14.00, isHot = true, isFood = true, isPremium = false, isAlcoholic = false)
  val quiche = menu("Quiche Lorraine", 9.00, isHot = false, isFood = true, isPremium = false, isAlcoholic = false)

  val coqAuVin = menu("Coq au vin", 18.50, isHot = true, isFood = true, isPremium = true, isAlcoholic = false)
  val lobster = menu("Lobster", 25.00, isHot = true, isFood = true, isPremium = true, isAlcoholic = false)
  val caviar = menu("Caviar", 30.00, isHot = false, isFood = true, isPremium = true, isAlcoholic = false)


  //INSTANTIATING CUSTOMERS
  val connie = Customer("Connie", 4, 22)
  val cristian = Customer("Cristian", 2, 15)
  val sarina = Customer("Sarina", 3, 23)
  val jake = Customer("Jake", 6, 12)
  val robyn = Customer("Robyn", 6, 24)
  val yonis = Customer("Yonis", 8, 18)


  //basic bill
  def billCalculator(customer: Customer, items: List[menu]) = {
    if(premiumFood(items)){
    items.map(item => item.cost).sum}
    else {
      (items.map(item => item.cost).sum) - loyaltyDiscount(customer, items)
    }
  }

  //calculating discount based on loyalty stars
  def loyaltyDiscount(customer: Customer, items: List[menu]): BigDecimal =
    customer.loyaltyStars match {
    case (0) => 1 * (items.map(item => item.cost).sum)
    case (1) => 1 * (items.map(item => item.cost).sum)
    case (2) => 1 * (items.map(item => item.cost).sum)
    case (3) => .025 * (items.map(item => item.cost).sum)
    case (4) => .05 * (items.map(item => item.cost).sum)
    case (5) => .075 * (items.map(item => item.cost).sum)
    case (6) => .1 * (items.map(item => item.cost).sum)
    case (7) => .125 * (items.map(item => item.cost).sum)
    case (8) => .15 * (items.map(item => item.cost).sum)
  }

  //loyalty card points increase if spent over £20
  def loyaltyPoints(customer: Customer, items: List[menu]): String = {
        if (billCalculator(customer, items) >= 20 && customer.loyaltyStars < 8){
      customer.loyaltyStars += 1
      s"Loyalty point added! Current total: ${customer.loyaltyStars}"
    } else if (billCalculator(customer, items) >= 20 && customer.loyaltyStars >= 8){
      "Maximum loyalty points reached! Congratulations you receive a 20% discount on all non-premium orders. "}
    else "Spend at least £20 next time to get a loyalty point"
  }

  //method to check if the order is only drinks
  def onlyDrinks(items: List[menu]) ={
    !items.exists(items => items.isFood)
  }

  //method to check whether order contains hot food
  def hotFood(items: List[menu]): Boolean ={
    val hotFood = items.filter(item => (item.isFood) && (item.isHot))
//    println(hotFood)
    if (hotFood.isEmpty){
      false
    } else true
  }

  //method to check whether food is premium
  def premiumFood(items: List[menu]): Boolean ={
    val premiumFood = items.filter(item => item.isPremium)
    if (premiumFood.isEmpty){
      false
    } else true
  }

  //calculate a 10% service charge with £20 cap
  def ten(customer: Customer, items: List[menu]): BigDecimal ={
    val serviceCharge = (items.map(item => item.cost).sum) * 0.1
    if (serviceCharge >= 20) {20}
    else serviceCharge
  }

  //calculate a 20% service charge with £20 cap
  def twenty(customer: Customer, items: List[menu]): BigDecimal ={
    val serviceCharge = (items.map(item => item.cost).sum) * .2
    if (serviceCharge >= 20) {20}
    else serviceCharge
  }

  //calculate a 25% service charge with £40 cap for premium food
  def twentyFive(customer: Customer, items: List[menu]): BigDecimal ={
    val serviceCharge = (items.map(item => item.cost).sum) * 0.25
    if (serviceCharge >= 40) {40}
    else serviceCharge
  }

  //Happy Hour
//  def happyHour(items: List[menu]): BigDecimal = {
//    val time = LocalTime.now.getHour
//    if (time > 15 && time < 22){
//      items.filter(items => items.isAlcoholic).map(drinks => drinks.cost).sum /2
//    } else 0
//  }

//  bill with VAT
  def VAT(customer: Customer, items: List[menu]): String = {
//    happyHour(items)
    val initialPrice = billCalculator(customer, items)
    val discount = (loyaltyDiscount(customer, items))

    println("Thank you for ordering at X Cafe!")
    println("-----------------------------------------------------")
    println(loyaltyPoints(customer, items))
    println("-----------------------------------------------------")
   println("Your Order: \n" + items.map(food => food.item))
    println("-----------------------------------------------------")

    if (onlyDrinks(items)){s"Bill Total: £$initialPrice " +
      s"+ service charge £0 " +
      s"- £$discount loyalty discount"}
    else if (!onlyDrinks(items) && !hotFood(items) && !premiumFood(items))
      {s"Bill Total: £${initialPrice + ten(customer, items)}. " +
        s"Order total: ${initialPrice + discount} " +
        s" + Service charge: £ ${ten(customer, items)}  " +
        s" - £$discount loyalty discount"}
    else if (hotFood(items) && !premiumFood(items)){
      s"Bill Total: £${initialPrice + twenty(customer, items)}. " +
        s"Order total: ${initialPrice + discount}  " +
        s"+ service charge £ ${twenty(customer, items)} " +
        s"- £$discount loyalty discount"
    } else if(premiumFood(items)){
    s"Bill Total: £${initialPrice + twentyFive(customer, items)}. ${initialPrice + discount} with service charge £ ${twentyFive(customer, items)} and £$discount loyalty discount"
    }
    else s"Error calculating total cost"
  }


//  println(VAT(connie, List(caviar)))
//  println("-----------------------------------------------------")
//  println("first order")
//  println(VAT(jake, List(steakFrites, onionSoup, ratatouille)))
//  println("-----------------------------------------------------")
//  println(VAT(yonis, List(caviar)))
//  println("-----------------------------------------------------")
//  println(VAT(robyn, List(onionSoup, cola, Ratatouille)))
//  println("-----------------------------------------------------")
  println(VAT(sarina, List(whiteWine, redWine, cola, beer)))
//  println("second order")
//  println(VAT(jake, List(steakFrites, onionSoup, Ratatouille)))
//  println("-----------------------------------------------------")
//
//  println("third order")
//  println(VAT(jake, List(steakFrites, onionSoup, Ratatouille)))
//  println("-----------------------------------------------------")
//


}
