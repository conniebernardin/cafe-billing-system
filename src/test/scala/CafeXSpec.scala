import Currency.{EUR, GBP}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec


class CafeXSpec extends AnyWordSpec with Matchers {


  val coffee = MenuItem("Cafe", 1.00, isHot = true, isFood = false, isPremium = false)
  val whiteWine = MenuItem("Sauvignon Blanc", 4.00, isHot = false, isFood = false, isPremium = true)

  val cheeseSandwich = MenuItem("Cheese Sandwich", 2.00, isHot = false, isFood = true, isPremium = false)
  val steakFrites = MenuItem("Steak frites", 4.50, isHot = true, isFood = true, isPremium = false)
  val ratatouille = MenuItem("Ratatouille", 14.00, isHot = true, isFood = true, isPremium = false)
  val expensiveItem = MenuItem("item", 140.00, isHot = true, isFood = true, isPremium = false)

  val lobster = MenuItem("Lobster", 25.00, isHot = true, isFood = true, isPremium = true)

  val sarina = Customer("Sarina", 2)
  val jake = Customer("Jake", 8)

  case object CAD extends Currency

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

  "currencyExchangeRate" should {
    "return 1.18 rate when Euro input" in {
      assert(CafeX.currencyExchangeRate(EUR).equals(1.18))
    }
  }

  "currencyExchangeRate" should {
    "return x1 rate when unknown input" in {
      assert(CafeX.currencyExchangeRate(CAD).equals(1))
    }
  }

  "setCurrencySymbol" should {
    "return pound sign as string when GBP input as currency" in {
      assert(CafeX.setCurrencySymbol(GBP).equals("£"))
    }
  }

  "setCurrencySymbol" should {
    "return default pound sign as string when unknown input as currency" in {
      assert(CafeX.setCurrencySymbol(CAD).equals("£"))
    }
  }

  "loyaltyDiscount" should {
    "return sum of food and drink when customer has less than 3 points" in{
      assert(CafeX.loyaltyDiscount(sarina, List(ratatouille, ratatouille)).equals(28))
    }
  }

  "loyaltyDiscount" should {
    "return 15% of sum of food and drink when customer has 8 points" in{
      assert(CafeX.loyaltyDiscount(jake, List(ratatouille, ratatouille)).equals(4.2))
    }
  }

  "loyaltyDiscount" should {
    "return sum of food and drink if customer has over 8 points" in{
      assert(CafeX.loyaltyDiscount(Customer("customer", 9), List(ratatouille, ratatouille)).equals(28))
    }
  }

  "sumMenuItems" should {
    "return the sum of all menu items" in {
      assert(CafeX.sumMenuItems(List(ratatouille, ratatouille, coffee)).equals(29))
    }
  }

  "billWithLoyaltyDiscount" should {
    "return total with loyalty discount applied if order doesn't contain premium items" in {
      assert(CafeX.billWithLoyaltyDiscount(jake, List(ratatouille, ratatouille)).equals(23.8))
    }
  }

  "serviceChargeCap" should {
    "return 0 if the order is only drinks" in {
      assert(CafeX.serviceChargeCap(List(coffee, coffee)).equals(0))
    }
  }

  "serviceChargeCap" should {
    "return 20 if the order service charge is over 20 and there's no premium items" in {
      assert(CafeX.serviceChargeCap(List(expensiveItem)).equals(20))
    }
  }

  "serviceChargeCap" should {
    "return 40 if the order service charge is over 20 and contains premium item" in {
      assert(CafeX.serviceChargeCap(List(expensiveItem, lobster)).equals(40))
    }
  }

  "serviceChargeCalculator" should {
    "return 10% of total order sum if it contains cold, non-premium food " in {
      assert(CafeX.serviceChargeCalculator(List(cheeseSandwich, cheeseSandwich)).equals(.40))
    }
  }

  "serviceChargeCalculator" should {
    "return 20% of total order sum if it contains hot, non-premium food " in {
      assert(CafeX.serviceChargeCalculator(List(steakFrites)).equals(.90))
    }
  }

  "serviceChargeCalculator" should {
    "return 40% of total order sum if it contains premium food " in {
      assert(CafeX.serviceChargeCalculator(List(lobster, lobster)).equals(12.5))
    }
  }

}



