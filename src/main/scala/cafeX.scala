object cafeX extends App {

  //MENU

  case class menu(item: String, cost: BigDecimal, isHot: Boolean) {
  }

  //INSTANTIATING ITEMS
  val cola = menu("Cola", .50, isHot = false)
  val coffee = menu("Coffee", 1.00, isHot = true)
  val cheeseSandwich = menu("Cheese Sandwich", 2.00, isHot = false)
  val steakSandwich = menu("Steak Sandwich", 4.50, isHot = true)

  def billCalculator(items: List[menu]) = items.map(item => item.cost).sum


  println(billCalculator(List(cola, cola, coffee, steakSandwich)))



}
