package TowerClimb



/** The abstract clas Item has four subtypes: Weapon, Armor, Consumable, and Buff. Each of which
  * have their own properties and definitions for the abstract methods 'typeOfItem' and 'description'. */

abstract class Item(val name: String, val desc: String, val price: Int) {


  /** Returns a short textual representation of the item (its name, that is). */
  override def toString = this.name + s" (${this.price}G)"


  /** Used to determine the instance class of a specific Item.
    * [Implementations of this method can all be replaced by 'isInstanceOf[Class]'.] */
  def typeOfItem: String
  def description: String


}


/** The classes Weapon and Armor each have a string variable for the type of stat that attackDamage or defenceAmount is.
  * Implementations of this property can be improved by implementing another class of different attack types. */

class Weapon(name: String, desc: String, price: Int, val attackType: Option[String], val attackDamage: Int) extends Item(name, desc, price) {


  def typeOfItem = "Weapon"


  def stats = this.name + s" (${this.attackDamage} ${this.returnModifiers})"


  def returnModifiers = attackType match {
    case Some("physical") => "[PHYSICAL]"
    case Some("magic") => "[MAGIC]"
    case _ => "[BASIC]"
  }


  def description = "[Offensive] " + this.name + s": ${this.attackDamage} ATCK " + this.returnModifiers + s" (${this.price}G)"


}


class Armor(name: String, desc: String, price: Int, val defenceType: Option[String], val defenceAmount: Int) extends Item(name, desc, price) {


  def typeOfItem = "Armor"


  def stats = this.name + s" (${this.defenceAmount} ${this.returnModifiers})"


  def returnModifiers = defenceType match {
    case Some("physical") => "[PHYSICAL]"
    case Some("magic") => "[MAGIC]"
    case _ => "[BASIC]"
  }


  def description = "[Defensive] " + this.name + s": ${this.defenceAmount} DEF " + this.returnModifiers + s" (${this.price}G)"


}


class Consumable(name: String, desc: String, price: Int, val amount: Int) extends Item(name, desc, price) {


  def typeOfItem = "Consumable"


  def description = "[Consumable] " + this.name + s": ${this.amount} HP" + s" (${this.price}G)"


}


class Buff(name: String, desc: String, price: Int, val amount: Int, val buff: String) extends Item(name, desc, price) {


  def typeOfItem = "Buff"


  def description = s"[${this.buff} Buff] " + this.name + s": ${this.amount} ${this.buff}" + s" (${this.price}G)"


}