
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
  val Ratatouille = MenuItem("Ratatouille", 14.00, isHot = true, isFood = true, isPremium = false)
  val Quiche = MenuItem("Quiche Lorraine", 9.00, isHot = false, isFood = true, isPremium = false)

  val coqAuVin = MenuItem("Coq au vin", 18.50, isHot = true, isFood = true, isPremium = true)
  val lobster = MenuItem("Lobster", 25.00, isHot = true, isFood = true, isPremium = true)
  val caviar = MenuItem("Caviar", 30.00, isHot = false, isFood = true, isPremium = true)


  val connie = Customer("Connie", 4)
  val cristian = Customer("Cristian", 2)
  val sarina = Customer("Sarina", 3)
  val jake = Customer("Jake", 6)
  val robyn = Customer("Robyn", 6)
  val yonis = Customer("Yonis", 8)


  private def sumMenuItems(menuItems: List[MenuItem]): BigDecimal = menuItems.map(item => item.cost).sum

  def IncreaseLoyaltyPoints(customer: Customer, items: List[MenuItem]): String = { //TODO: You've worked out the loyalty points and returned a messages, testing this could prove difficult. Next time return the points and construct the message elsewhere
    if (billWithLoyaltyDiscount(customer, items) >= 20 && customer.loyaltyStars < 8){
      //customer.loyaltyStars += 1 //TODO: This type of mutability strays away from what scala offers, try to do this with immutability
      s"Loyalty point added! Current total: ${customer.loyaltyStars}"
    } else if (billWithLoyaltyDiscount(customer, items) >= 20 && customer.loyaltyStars >= 8){
      "Maximum loyalty points reached! Congratulations you receive a 20% discount on all non-premium orders. "}
    else "Spend at least £20 next time to get a loyalty point"
  }

  def loyaltyDiscount(customer: Customer, items: List[MenuItem]): BigDecimal =
    customer.loyaltyStars match {
    case (0) => 1 * sumMenuItems(items)
    case (1) => 1 * sumMenuItems(items)
    case (2) => 1 * sumMenuItems(items)
    case (3) => .025 * sumMenuItems(items)
    case (4) => .05 * sumMenuItems(items)
    case (5) => .075 * sumMenuItems(items)
    case (6) => .1 * sumMenuItems(items)
    case (7) => .125 * sumMenuItems(items)
    case (8) => .15 * sumMenuItems(items)
  }

  def billWithLoyaltyDiscount(customer: Customer, items: List[MenuItem]) = {
    if(premiumFood(items)){ //TODO: premiumFood isn't very descriptive, I know this is picky but "isPremiumFood" `sounds` nicer
      sumMenuItems(items) }
    else {
      sumMenuItems(items) - loyaltyDiscount(customer, items)
    }
  }

  def onlyDrinks(items: List[MenuItem]) = !items.exists(items => items.isFood)

  def hotFood(items: List[MenuItem]): Boolean = items.exists(item => item.isHot && item.isFood)

  def premiumFood(items: List[MenuItem]): Boolean = items.exists(item => item.isPremium)


  def serviceChargeCalculator(items: List[MenuItem]):BigDecimal ={
    if(onlyDrinks(items)){
      0
    } else if (!onlyDrinks(items) && !hotFood(items) && !premiumFood(items)){
      sumMenuItems(items) * 0.1
    } else if (hotFood(items) && !premiumFood(items)){
      sumMenuItems(items) * 0.2
    } else if (premiumFood(items)){
      sumMenuItems(items) * 0.25
    } else 0
  }

  def serviceChargeCap(items: List[MenuItem]): BigDecimal = {
    if (serviceChargeCalculator(items) <= 20){
      serviceChargeCalculator(items)
    } else if (serviceChargeCalculator(items) >= 20 && !items.exists(items => items.isPremium)){
      20
    }  else if (serviceChargeCalculator(items) > 40 && items.exists(items => items.isPremium)) {
     40
    } else serviceChargeCalculator(items)
  }

//  bill with VAT TODO: "billWithVAT" looks like a much better name than VAT...
  def VAT(customer: Customer, items: List[MenuItem]): String = { //TODO: We don't capitalize defs, e.g. def vat(...)...
    val initialPrice = billWithLoyaltyDiscount(customer, items) //TODO: isn't bill calculator already taking off the loyalty discount?
    val serviceCharge = serviceChargeCalculator(items)
    println("Thank you for ordering at X Cafe!")
    println("-----------------------------------------------------")
    println(IncreaseLoyaltyPoints(customer, items))
    println("-----------------------------------------------------")
   println("Your Order: \n" + items.map(food => food.item))
    println("-----------------------------------------------------")

    s"Total: ${initialPrice - serviceCharge}" +
      s"Order total: ${sumMenuItems(items)}" +
      s"Loyalty Discount: ${loyaltyDiscount(customer, items)}" +
      s"Service Charge: ${serviceCharge}"
  }



  println(VAT(jake, List(steakFrites, onionSoup)))
  println("-----------------------------------------------------")

}
