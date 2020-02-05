package yoppworks.hackerchallenges.bundlepricing

import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain.{CartItem, Quantity}


/**
  * Contains implementation of the domain, including The cart, cart items, price & quantity units, and the logic to create some promotions
  *
  * DO NOT EDIT
  *
  * These specs are used by Tests
  */
object BundlePricingDomain {

  // Cart data
  case class CatalogItem(name: String, unitPrice: Price)
  case class Cart(cartItemsParam: Seq[CartItem]) { val cartItems: Seq[CartItem] = BundlePricingUtil.groupCartItem(cartItemsParam) }
  case class CartItem(catalogItem: CatalogItem, quantity: Quantity){
        val cartItemTotalPrice: Int = catalogItem.unitPrice.value * quantity.value
  }
  object InvalidCartException extends Exception


  // Wrapper type for Units
  class Quantity private (val value: Int) extends AnyVal { //  1 <= Integer <= 99
    override def toString: String = "Qt " + value.toString
  }
  
  object Quantity {
    def apply(value: Int): Quantity = {
      if(value >= 1 && value <= 99) new Quantity(value)
      else throw new Exception(value + " isn't a valid Quantity : 1 <= Integer <= 99")
    }
  }
  
  class Price private (val value: Int) extends AnyVal { //  0 <= Integer
    // as price is in cents example 2250 => $22.50, need to convert in nice looking price
    override def toString: String = value.toString.dropRight(2) + "." + value.toString.takeRight(2)
  }
  
  object Price {
    def apply(value: Int): Price = if(value >= 0) new Price(value) else throw new Exception(value + " isn't a valid Price : 0 <= Integer")
  }


  // Bundle promotions specs,
  // Each bundle should implement this trait
  sealed trait BundlePromotion {
    def cartItems: Seq[CartItem] // the items of the bundle, example 2 apples + 1 margarine, it's how we detect the bundle pattern
    def totalDiscountedPrice: Price // total price of the bundle, we don't need unit price of each item,
                                    // total price is enough as it's this value which determine if bundle reduce or not the price of the cart
  }

}


// Some implementations of promotions
object BundlePromotions {
  import BundlePricingDomain._

  /**
    Discount on total price
    example 1 apple $2.00 , 2 apples for $3.00
   */
  class BundleTotalPriceDiscount(
   cartItemsParams: Seq[CartItem], // how we detect the pattern
   totalPrice: Price // the discount
   ) extends BundlePromotion {
    def cartItems: Seq[CartItem] = BundlePricingUtil.groupCartItem(cartItemsParams)
    def totalDiscountedPrice: Price = totalPrice
    override def toString = cartItems + " => Total " + totalDiscountedPrice
  }
  
  object BundleTotalPriceDiscount {
    def apply(cartItemsParams: Seq[CartItem],totalPrice: Price) = new BundleTotalPriceDiscount(cartItemsParams, totalPrice)
  }

  /**
    Discount on specific item(s) (we override the unit price) into a promotion
    example a loaf of bread “A” purchased with two sticks of margarine “B” and
    the second stick of margarine is free (e.g. $0)
   */
  class BundleDiscountOnItemUnitPrice(discountedItems: Seq[MaybeDiscountedItem]) extends BundlePromotion {
    
    def cartItems: Seq[CartItem] = BundlePricingUtil.groupCartItem(discountedItems.map(_.cartItem))
    
    def totalDiscountedPrice: Price = {
      val itemsPrices = discountedItems.map{itemWrapper => itemWrapper.optionalUnitPriceOverride match {
          case Some(discountedPrice) =>
            discountedPrice.value * itemWrapper.cartItem.quantity.value
          case None => // normal price
            itemWrapper.cartItem.catalogItem.unitPrice.value * itemWrapper.cartItem.quantity.value
        }
      }
      Price(itemsPrices.sum)
    }
    override def toString: String = discountedItems.toString()
  }
  
  
  object BundleDiscountOnItemUnitPrice {
    def apply(discountedItems: Seq[MaybeDiscountedItem]) = new BundleDiscountOnItemUnitPrice(discountedItems)
  }
  
  case class MaybeDiscountedItem(cartItem: CartItem, optionalUnitPriceOverride: Option[Price])


}

object BundlePricingUtil {

  def groupCartItem(cartItems: Seq[CartItem]): Seq[CartItem] = {
    // check if all items are well grouped and group them
    // Example (Apple, qt 1) (Apple, qt 1) become (Apple, qt 2)
    cartItems.groupBy(_.catalogItem).map{
      case (item, toGroupItems) => CartItem(item, Quantity(toGroupItems.map(_.quantity.value).sum))
    }.toSeq
  }

}
