

object cafeX extends App {

  //MENU

  case class menu(item: String, cost: BigDecimal, isHot: Boolean, isFood: Boolean) {
  }

  //INSTANTIATING ITEMS
  val cola = menu("Cola", .50, isHot = false, isFood = false)
  val coffee = menu("Coffee", 1.00, isHot = true, isFood = false)
  val cheeseSandwich = menu("Cheese Sandwich", 2.00, isHot = false, isFood = true)
  val steakSandwich = menu("Steak Sandwich", 4.50, isHot = true, isFood = true)

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


//  bill with VAT
  def VAT(items: List[menu]): BigDecimal = {
    val initialPrice = billCalculator(items: List[menu])

    if (onlyDrinks(items)){initialPrice}
    else if (!onlyDrinks(items) && !hotFood(items))
      {initialPrice + ten(items)}
    else if (hotFood(items)){
      initialPrice + twenty(items)
    } else initialPrice

  }

//println("Should be No VAT: 1.5 ")
//println(VAT(List(cola, coffee)))
//
//  println("Should be 10% VAT: 2.75 ")
//println(VAT(List(cola, cheeseSandwich)))
//
//  println("Should be 20% VAT: 8.40")
//  println(VAT(List(cola, cheeseSandwich, steakSandwich)))


//  println("is there any hot food?")
//  println(hotFood(List( cola, cheeseSandwich, coffee)))
//  println(hotFood(List(coffee, cola, steakSandwich)))
  /*When all purchased items are drinks no service charge is applied
When purchased items include any food apply a service charge of 10% to the total bill (rounded to 2 decimal places)
When purchased items include any hot food apply a service charge of 20% to the total bill with a maximum £20 service charge
*/

}
