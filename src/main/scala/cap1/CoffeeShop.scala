package cap1

object CoffeeShop {

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val coffee = Coffee()
    (coffee, Charge(cc, coffee.price))
  }

  def buyCoffees(cc: CreditCard, n: Integer): (List[Coffee], Charge) = {
    val purchases = List.fill(n)(buyCoffee(cc))
    purchases.unzip match {
      case (coffees, charges) =>
        (coffees, charges.reduce(_.combine(_)))
    }
  }

  case class CreditCard()

  case class Charge(creditCard: CreditCard, amount: Long) {
    def combine(other: Charge) = {
      Charge(creditCard, other.amount + this.amount)
    }
  }

  case class Coffee(price: Long = 500l)

  def main(args: Array[String]) {
    val cc = CreditCard()

    println(buyCoffee(cc))
    println(buyCoffees(cc, 10))
  }

}

