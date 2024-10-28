
case class Item(id: String, name: String, price: Double)

case class Order(orderId: String, items: List[Item], discountCode: Option[String])

object OrderProcessing {

  def calculateTotal(order: Order): Double = {
    order.items.map(_.price).sum
  }

  def applyDiscount(order: Order, discountRate: Double): Order = {
    val discountedItems = order.items.map(item => item.copy(price = item.price * (1 - discountRate)))
    order.copy(items = discountedItems)
  }

  def processOrders(orders: List[Order], discounts: Map[String, Double]): List[Double] = {
    orders.map { order =>
      val discountRate = order.discountCode.flatMap(discounts.get).getOrElse(0.0)
      val discountedOrder = applyDiscount(order, discountRate)
      calculateTotal(discountedOrder)
    }
  }
}

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or
// click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit =
  val items = List(Item("1", "item1", 10.0), Item("2", "item2", 15.0))
  val order = Order("order1", items, None)
  val discount = OrderProcessing.applyDiscount(order, 0.25);
  val total = OrderProcessing.calculateTotal(discount)
  println(total)
