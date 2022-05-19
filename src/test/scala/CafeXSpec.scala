import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec


class CafeXSpec extends AnyWordSpec with Matchers {


  val coffee = MenuItem("Cafe", 1.00, isHot = true, isFood = false, isPremium = false)
  val whiteWine = MenuItem("Sauvignon Blanc", 4.00, isHot = false, isFood = false, isPremium = true)

  val cheeseSandwich = MenuItem("Cheese Sandwich", 2.00, isHot = false, isFood = true, isPremium = false)
  val steakFrites = MenuItem("Steak frites", 4.50, isHot = true, isFood = true, isPremium = false)
  val ratatouille = MenuItem("Ratatouille", 14.00, isHot = true, isFood = true, isPremium = false)

  val lobster = MenuItem("Lobster", 25.00, isHot = true, isFood = true, isPremium = true)


  "checkIfOrderIsOnlyDrinks" should {
    "return false if order contains food" in {
      val order: List[MenuItem] = List(whiteWine, cheeseSandwich )

      assert(CafeX.checkIfOrderIsOnlyDrinks(order).equals(false))

    }
  }

  "checkIfOrderIsOnlyDrinks" should {
    "return true if order only contains drinks" in {
      val order: List[MenuItem] = List(whiteWine, coffee )

      assert(CafeX.checkIfOrderIsOnlyDrinks(order).equals(true))

    }
  }

}
