

object cafeX extends App {

  //MENU

  case class menu(item: String, cost: BigDecimal, isHot: Boolean, isFood: Boolean, isPremium: Boolean) {
  }

  //INSTANTIATING ITEMS
  val cola = menu("Cola", .50, isHot = false, isFood = false, isPremium = false)
  val coffee = menu("Coffee", 1.00, isHot = true, isFood = false, isPremium = false)
  val cheeseSandwich = menu("Cheese Sandwich", 2.00, isHot = false, isFood = true, isPremium = false)
  val steakSandwich = menu("Steak Sandwich", 4.50, isHot = true, isFood = true, isPremium = false)
  val lobster = menu("Lobster", 25.00, isHot = true, isFood = true, isPremium = true)

  //basic bill
  def billCalculator(items: List[menu]) = {
    items.map(item => item.cost).sum
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
  def ten(items: List[menu]): BigDecimal ={
    val serviceCharge = billCalculator(items: List[menu]) * .1
    if (serviceCharge >= 20) {20}
    else serviceCharge
  }

  //calculate a 20% service charge with £20 cap
  def twenty(items: List[menu]): BigDecimal ={
    val serviceCharge = billCalculator(items: List[menu]) * .2
    if (serviceCharge >= 20) {20}
    else serviceCharge
  }

  def twentyFive(items: List[menu]): BigDecimal ={
    val serviceCharge = billCalculator(items: List[menu]) * .25
    if (serviceCharge >= 40) {40}
    else serviceCharge
  }


//  bill with VAT
  def VAT(items: List[menu]): String = {
    val initialPrice = billCalculator(items: List[menu])

    if (onlyDrinks(items)){s"Bill Total: £$initialPrice with service charge £0"}
    else if (!onlyDrinks(items) && !hotFood(items) && !premiumFood(items))
      {s"Bill Total: £$initialPrice with service charge £ ${ten(items)}"}
    else if (hotFood(items) && !premiumFood(items)){
      s"Bill Total: £$initialPrice with service charge £ ${twenty(items)}"
    } else if(premiumFood(items)){
    s"Bill Total: £$initialPrice with service charge £ ${twentyFive(items)}"
    }
    else s"Error calculating total cost"

  }

//println("Should be No VAT: 1.5 ")
println(VAT(List(cola, coffee)))
//
//  println("Should be 10% VAT: 2.75 ")
println(VAT(List(cola, cheeseSandwich)))
//
//  println("Should be 20% VAT: 8.40")
  println(VAT(List(cola, cheeseSandwich, steakSandwich)))
  println(VAT(List(cola, coffee, steakSandwich, cheeseSandwich, lobster, lobster, lobster, lobster, lobster, steakSandwich)))


//  println("is there any hot food?")
//  println(hotFood(List( cola, cheeseSandwich, coffee)))
//  println(hotFood(List(coffee, cola, steakSandwich)))
  /*When all purchased items are drinks no service charge is applied
When purchased items include any food apply a service charge of 10% to the total bill (rounded to 2 decimal places)
When purchased items include any hot food apply a service charge of 20% to the total bill with a maximum £20 service charge
*/

}
