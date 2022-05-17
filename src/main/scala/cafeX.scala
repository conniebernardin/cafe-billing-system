
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


  private def sumMenuItems(menuItems: List[MenuItem]): BigDecimal =
    menuItems.map(item => item.cost).sum

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

  def onlyDrinks(items: List[MenuItem]) =
    !items.exists(items => items.isFood)

  def hotFood(items: List[MenuItem]): Boolean =
    items.exists(item => item.isHot && item.isFood)

  def premiumFood(items: List[MenuItem]): Boolean = {
    items.exists(item => item.isPremium)
  }

  //TODO: ten(), twenty(), and twentyFive() look awfully similar, is there a way you can see to coombine them into one function?
  //calculate a 10% service charge with £20 cap
  def tenPercentOffItems(customer: Customer, items: List[MenuItem]): BigDecimal ={ //TODO: Any reason customer is passed in here as a parameter
    val serviceCharge = (items.map(item => item.cost).sum) * 0.1
    if (serviceCharge >= 20)
      20
    else
      serviceCharge
  }

  //calculate a 20% service charge with £20 cap
  def twenty(customer: Customer, items: List[MenuItem]): BigDecimal ={
    val serviceCharge = (items.map(item => item.cost).sum) * .2
    if (serviceCharge >= 20)
      20
    else
      serviceCharge
  }

  //calculate a 25% service charge with £40 cap for premium food
  def twentyFive(customer: Customer, items: List[MenuItem]): BigDecimal ={
    val serviceCharge = (items.map(item => item.cost).sum) * 0.25
    if (serviceCharge >= 40)
      40
    else
      serviceCharge
  }

//  bill with VAT TODO: "billWithVAT" looks like a much better name than VAT...
  def VAT(customer: Customer, items: List[MenuItem]): String = { //TODO: We don't capitalize defs, e.g. def vat(...)...
    val initialPrice = billWithLoyaltyDiscount(customer, items) //TODO: isn't bill calculator already taking off the loyalty discount?
    val discount = (loyaltyDiscount(customer, items))
    println("Thank you for ordering at X Cafe!")
    println("-----------------------------------------------------")
    println(IncreaseLoyaltyPoints(customer, items))
    println("-----------------------------------------------------")
   println("Your Order: \n" + items.map(food => food.item))
    println("-----------------------------------------------------")

    if (onlyDrinks(items)){s"Bill Total: £$initialPrice " +
      s"+ service charge £0 " +
      s"- £$discount loyalty discount"}
    else if (!onlyDrinks(items) && !hotFood(items) && !premiumFood(items))
      {s"Bill Total: £${initialPrice + tenPercentOffItems(customer, items)}. " +
        s"Order total: ${initialPrice + discount} " +
        s" + Service charge: £ ${tenPercentOffItems(customer, items)}  " +
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
  } //TODO: Constructing and returning these Strings are great but you've made it hard to test


//  println(VAT(connie, List(caviar)))
//  println("-----------------------------------------------------")
  println(VAT(jake, List(steakFrites, onionSoup)))
  println("-----------------------------------------------------")
//  println(VAT(yonis, List(caviar)))
//  println("-----------------------------------------------------")
//  println(VAT(robyn, List(onionSoup, cola, Ratatouille)))
//  println("-----------------------------------------------------")
//  println(VAT(sarina, List(whiteWine, redWine, cola, beer)))



}
