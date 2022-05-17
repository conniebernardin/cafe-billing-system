
object CafeX extends App {
  //MENU
  //THEME: FRENCH CAFE

  //TODO: Some of these should be vals, some Objects and some Classes... possibly even a trait lying about somewhere?
  //INSTANTIATING ITEMS
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


  //INSTANTIATING CUSTOMERS
  val connie = Customer("Connie", 4)
  val cristian = Customer("Cristian", 2)
  val sarina = Customer("Sarina", 3)
  val jake = Customer("Jake", 6)
  val robyn = Customer("Robyn", 6)
  val yonis = Customer("Yonis", 8)


  //loyalty card points increase if spent over £20 //TODO: We never comment for defs, the name of the method should explain exactly what it does so these aren't needed
  def loyaltyPoints(customer: Customer, items: List[MenuItem]): String = { //TODO: You've worked out the loyalty points and returned a messages, testing this could prove difficult. Next time return the points and construct the message elsewhere
    if (billCalculator(customer, items) >= 20 && customer.loyaltyStars < 8){
      //customer.loyaltyStars += 1 //TODO: This type of mutability strays away from what scala offers, try to do this with immutability
      s"Loyalty point added! Current total: ${customer.loyaltyStars}"
    } else if (billCalculator(customer, items) >= 20 && customer.loyaltyStars >= 8){
      "Maximum loyalty points reached! Congratulations you receive a 20% discount on all non-premium orders. "}
    else "Spend at least £20 next time to get a loyalty point"
  }


  //basic bill
  def billCalculator(customer: Customer, items: List[MenuItem]) = {
    if(premiumFood(items)){ //TODO: premiumFood isn't very descriptive, I know this is picky but "isPremiumFood" `sounds` nicer
    items.map(item => item.cost).sum}
    else {
      (items.map(item => item.cost).sum) - loyaltyDiscount(customer, items)
    }
  }

  //calculating discount based on loyalty stars
  def loyaltyDiscount(customer: Customer, items: List[MenuItem]): BigDecimal = //TODO: Great naming! Since this is returning a loyaltyDiscount
    customer.loyaltyStars match { //TODO: Good use of pattern matching over if statements
    case (0) => 1 * sumMenuItems(items) //TODO: These all look ver similar, could we move them to a common function for readability?
    case (1) => 1 * (items.map(item => item.cost).sum)
    case (2) => 1 * (items.map(item => item.cost).sum)
    case (3) => .025 * (items.map(item => item.cost).sum)
    case (4) => .05 * (items.map(item => item.cost).sum)
    case (5) => .075 * (items.map(item => item.cost).sum)
    case (6) => .1 * (items.map(item => item.cost).sum)
    case (7) => .125 * (items.map(item => item.cost).sum)
    case (8) => .15 * (items.map(item => item.cost).sum)
  }

  private def sumMenuItems(menuItems: List[MenuItem]): BigDecimal =
    menuItems.map(item => item.cost).sum

  //method to check if the order is only drinks
  def onlyDrinks(items: List[MenuItem]) =
    !items.exists(items => items.isFood) //TODO: Really good use of scala's built in functions, you should use exists below!

  //method to check whether order contains hot food
  def hotFood(items: List[MenuItem]): Boolean =  //TODO: There already "exists" a function that does this method in scala...
    items.exists(item => item.isHot && item.isFood)

  //method to check whether food is premium
  def premiumFood(items: List[MenuItem]): Boolean = {//TODO: There already "exists" a function that does this method in scala...
    val premiumFood = items.filter(item => item.isPremium)
    if (premiumFood.isEmpty){ //TODO: This could simply be premiumFood.isEmpty, as it returns a Boolean type
      false
    } else true
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
    val initialPrice = billCalculator(customer, items) //TODO: isn't bill calculator already taking off the loyalty discount?
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
