
object cafeX extends App {

  //MENU

  case class Customer(name: String, var loyaltyStars: Int)

  case class menu(item: String, cost: BigDecimal, isHot: Boolean, isFood: Boolean, isPremium: Boolean)

  //INSTANTIATING ITEMS
  val cola = menu("Cola", .50, isHot = false, isFood = false, isPremium = false)
  val coffee = menu("Coffee", 1.00, isHot = true, isFood = false, isPremium = false)
  val cheeseSandwich = menu("Cheese Sandwich", 2.00, isHot = false, isFood = true, isPremium = false)
  val steakSandwich = menu("Steak Sandwich", 4.50, isHot = true, isFood = true, isPremium = false)
  val lobster = menu("Lobster", 25.00, isHot = true, isFood = true, isPremium = true)
  val caviar = menu("Caviar", 100.00, isHot = true, isFood = true, isPremium = false)

  //INSTANTIATING CUSTOMERS
  val connie = Customer("Connie", 4)
  val sarina = Customer("Sarina", 3)
  val jake = Customer("Jake", 6)

  //loyalty card tally
  def loyaltyPoints(customer: Customer, items: List[menu]): String = {
    if (billCalculator(customer, items) >= 20){
      customer.loyaltyStars += 1
      s"Loyalty point added! Current total: ${customer.loyaltyStars}"
    } else "Spend at least £20 next time to get a loyalty point"
  }


  //basic bill
  def billCalculator(customer: Customer, items: List[menu]) = {
    if(premiumFood(items)){
    items.map(item => item.cost).sum}
    else {
      (items.map(item => item.cost).sum) - loyaltyDiscount(customer, items)
    }
  }

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

  def twentyFive(customer: Customer, items: List[menu]): BigDecimal ={
    val serviceCharge = (items.map(item => item.cost).sum) * 0.25
    if (serviceCharge >= 40) {40}
    else serviceCharge
  }

//  bill with VAT
  def VAT(customer: Customer, items: List[menu]): String = {
    val initialPrice = billCalculator(customer, items)
    val discount = (loyaltyDiscount(customer, items))
    println(loyaltyPoints(customer, items))

    if (onlyDrinks(items)){s"Bill Total: £$initialPrice with service charge £0 and £$discount loyalty discount"}
    else if (!onlyDrinks(items) && !hotFood(items) && !premiumFood(items))
      {s"Bill Total: £${initialPrice + ten(customer, items)}. ${initialPrice + discount} with service charge £ ${ten(customer, items)} and £$discount loyalty discount"}
    else if (hotFood(items) && !premiumFood(items)){
      s"Bill Total: £${initialPrice + twenty(customer, items)}. ${initialPrice + discount} with service charge £ ${twenty(customer, items)} and £$discount loyalty discount"
    } else if(premiumFood(items)){
    s"Bill Total: £${initialPrice + twentyFive(customer, items)}. ${initialPrice + discount} with service charge £ ${twentyFive(customer, items)} and £$discount loyalty discount"
    }
    else s"Error calculating total cost"
  }


  println(VAT(connie, List(caviar)))
  println(VAT(jake, List(steakSandwich, steakSandwich)))
  println(VAT(jake, List(caviar)))



}
