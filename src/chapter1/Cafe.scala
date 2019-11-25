package chapter1

class Cafe {

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip

    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }
}

case class Charge(cc: CreditCard, amount: Int) {
  def combine(other: Charge): Charge =
    if (cc == other.cc) {
      Charge(cc, amount + other.amount)
    } else throw new Exception("Can't combine charges to a different cards")

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc)
      .values
      .map(_ reduce (_ combine _))
      .toList
}

class CreditCard {

  def charge(amount: Int): Unit = {

  }
}

class Payments {
  def charge(creditCard: CreditCard, amount: Int) = {

  }
}

class Coffee(val price: Int = 2)
