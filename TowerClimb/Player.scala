package TowerClimb

import scala.collection.mutable.Map
import scala.math._
import scala.util.Random


/** The Player class represents the controllable character in the game. */

class Player(startingArea: Area) {


  /** Some useful variables. */
  private var currentLocation = startingArea
  private var quitCommandGiven = false
  private val playerInventory = Map[String, Item]()


  /** PLAYER STATS BELOW: */
  private var currentHealthPoints = 500.0
  private var maxHealthPoints = 500.0
  private var basePhysicalAttack = 0
  private var baseMagicAttack = 0
  private var baseAttack = 0
  private var basePhysicalDefence = 0
  private var baseMagicDefence = 0
  private var baseDefence = 0
  private var luck = 100
  var wallet: Int = 200


  /** DEV COMMANDS: DO NOT USE UNLESS NECESSARY */
  def addAttack(x: Int) = {
    this.baseAttack += x
    s"$x ADDED TO BASEATTACK"
  }

  def addDefence(x: Int) = {
    this.baseDefence += x
    s"$x ADDED TO BASEDEFENCE"
  }

  def addGold(x: Int) = {
    this.wallet += x
    s"$x ADDED TO GOLD"
  }

  def heal = {
    this.currentHealthPoints = this.maxHealthPoints
    "FULLY HEALED"
  }

  def dev = {
    "\n\nThe dev commands are as follows: " +
      "\n\n/addattack      + amount [Int]          adds an amount of attack for the player" +
      "\n/adddefence     + amount [Int]          adds an amount of defence for the player" +
      "\n/addgold        + amount [Int]          adds an amount of gold for the player" +
      "\n/heal                                   fully heals the player"
  }


  /** Each piece of equipment adds a specific amount of stats to the player's effective stats. */
  private var equippedArmor = Map[String, Armor]("loincloth" -> new Armor("Loincloth", "I feel naked in this.", 50, None, 10))
  private var equippedWeapons = Map[String, Weapon]("broken wand" -> new Weapon("Broken Wand", "After failing to climb the tower last time, this is all I'm left with.", 50, None, 15))
  private var (boughtEquippables, boughtUsables) = (Vector[String](), Vector[String]())


  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Public method for a private variable. */
  def getLuck = this.luck


  /** Determines of the player's dead or not. */
  def hasDied = this.currentHealthPoints <= 0


  /** EFFECTIVE COMBAT STATS BELOW: */
  def attackBasic = this.equippedWeapons.values.filter(_.attackType.isEmpty).map(_.attackDamage).sum + this.baseAttack

  def attackPhysical = this.equippedWeapons.values.filter(n => n.attackType == Some("physical")).map(_.attackDamage).sum + this.basePhysicalAttack

  def attackMagic = this.equippedWeapons.values.filter(n => n.attackType == Some("magic")).map(_.attackDamage).sum + this.baseMagicAttack

  private def defenceNormal = this.equippedArmor.values.filter(n => n.defenceType.isEmpty).map(_.defenceAmount).sum + this.baseDefence

  private def defencePhysical = this.equippedArmor.values.filter(n => n.defenceType == Some("physical")).map(_.defenceAmount).sum + this.basePhysicalDefence

  private def defenceMagic = this.equippedArmor.values.filter(n => n.defenceType == Some("magic")).map(_.defenceAmount).sum + this.baseMagicDefence


  /** Returns a thorough description of the player's stats. */
  def stats: String = {
    s"HP: ${(this.currentHealthPoints * 10).ceil / 10.0} / ${this.maxHealthPoints}, LUCK: ${this.luck}, GOLD: ${this.wallet}G," +
    s"\nATTACK: ${this.attackBasic} | ${this.attackPhysical} | ${this.attackMagic} (basic | physical | magic)" +
    s"\nDEFENCE: ${this.defenceNormal} | ${this.defencePhysical} | ${this.defenceMagic}  (basic | physical | magic)" +
    s"\nPOWER LVL: ${this.statSum}, GREED: ${this.greed}"
  }


  /** Returns a measure of the player's power. */
  def statSum = (this.attackBasic + this.attackPhysical + this.attackMagic + this.defenceNormal + this.defencePhysical + this.defenceMagic + this.maxHealthPoints / 15 + this.luck / 5 - 70).toInt


  /** Returms a measure of the player's greed. */
  def greed = this.wallet + this.playerInventory.map(_._2.price).sum


  /** Defines how the player takes damage from a monster x times. Returns the HP value after the attack. */
  def takeDamage(monster: Monster, x: Int) = {
    def round(x: Double) = (x * 10).ceil / 10.0
    val before = round(this.currentHealthPoints)
    this.currentHealthPoints -= x * monster.attackPhysical * (1 - (this.defencePhysical + this.defenceNormal * 0.5) * 1.0 / (100 + this.defencePhysical + this.defenceNormal * 0.5))
    this.currentHealthPoints -= x * monster.attackMagic * (1 - (this.defenceMagic + this.defenceNormal * 0.5) * 1.0 / (100 + this.defenceMagic + this.defenceNormal * 0.5))
    if (monster.boss) this.currentHealthPoints -= x * (monster.attackPhysical + monster.attackMagic) * 1.0 / 2
    if (this.currentHealthPoints < 0) this.currentHealthPoints = 0
    s"Your HP: [${round(this.currentHealthPoints)} / ${this.maxHealthPoints}], lost ${round(before - round(this.currentHealthPoints))} HP"
  }


  /** Returns a boolean value for whether the player is in combat. */
  def isInCombat = this.currentLocation.combat.map(!_.isOver) == Some(true)


  /** Returns a description of the opposing monster's stats. */
  def statcheck: String = {
    if (this.currentLocation.combat.map(_.monster.isDefined) == Some(true)) {
      val monster = this.currentLocation.combat.get.monster.get
      s"${monster.name}: \nATTACK: ${monster.attackPhysical} | ${monster.attackMagic} (physical | magic) " +
        s"\nDEFENSE: ${monster.armorPhysical} | ${monster.armorMagic} (physical | magic)"
    } else "No monster to check."
  }


  /** Returns the description of the opposing monster. */
  def examine: String = {
    if (this.isInCombat) {
      this.currentLocation.combat.get.monster.get.description
    } else ""
  }


  /** Initiates combat if the player is on a non-empty arena. */
  def fight: String = {
    if (this.currentLocation.combat.map(_.outOfMonsters) == Some(true)) {
      "Nothing to fight here."
    } else if (this.currentLocation.arena && (this.currentLocation.combat.isEmpty || this.currentLocation.combat.map(_.isOver) == Some(true)) && this.currentLocation.combat.map(_.outOfMonsters) != Some(true)) {
      this.location.initiateCombat(this)
    } else {
      "You can only fight in the arena."
    }
  }


  /** Returns the current location of the player. */
  def location = this.currentLocation


  /** Attempts to move the player in a direction." */
  def go(direction: String) = {
    val area = this.location.neighbor(direction)
    def isLadder = area.map(_.neighbor("up").isDefined) == Some(true)
    val destination = if (isLadder && area.map(_.ladderunlocked) == Some(true)) area else if (!isLadder) area else None
    this.currentLocation = destination.getOrElse(this.currentLocation)
      if (this.currentLocation.floor == 5 && this.currentLocation.arena) {
        this.currentLocation.initiateCombat(this)
      }
    if (destination.isDefined) {if (area.map(_.floor == 5) == Some(true) && area.map(_.returnModifiers) == Some("Main Hall")) "You enter the Top Floor." else "You enter the " + area.get.returnModifiers + "."} else if (isLadder) "You have to defeat a monster first to access the ladder room." else "You can't go that direction."
  }


  /** Effectively quits the game. */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }


  /** Returns a "list" [String] of the items in the player's inventory. */
  def inventory = if (this.playerInventory.nonEmpty) "You are carryring:\n" + this.playerInventory.values.mkString(", ") else "You are empty-handed."


  /** Returns a boolean of whether the player has an item in their inventory or equipped slots. */
  def has(itemName: String) = this.playerInventory.contains(itemName.toLowerCase) || this.equippedWeapons.contains(itemName.toLowerCase) || this.equippedArmor.contains(itemName.toLowerCase)


  /** Returns description of the equipped items. */
  def equipped = {
    s"Armor: ${this.equippedArmor.values.map(_.stats).mkString(", ")} \nWeapon: ${this.equippedWeapons.values.map(_.stats).mkString(", ")}"
  }


  /** Equips an armor/weapon from the player's inventory, if the player has one */
  def equip(itemName: String): String = {
    val actualItemName = itemName.toLowerCase
    if (this.has(itemName) && this.playerInventory(actualItemName).typeOfItem != "Consumable" && this.playerInventory(actualItemName).typeOfItem != "Buff") {
      val item = this.playerInventory(actualItemName)
      item match {
        case x: Weapon => this.equippedWeapons += actualItemName -> item.asInstanceOf[Weapon]
        case x: Armor => this.equippedArmor += actualItemName -> item.asInstanceOf[Armor]
        case _ =>
      }
      this.playerInventory -= actualItemName
      s"You equipped ${item.name}."
    } else if (this.playerInventory.get(actualItemName).map(n => n.typeOfItem == "Consumbale" || n.typeOfItem == "Buff") == Some(true)) {
      "You can only equip weapons and armor."
    } else {
      s"You don't have ${itemName}."
    }
  }


  /** Equips the last Weapon/Armor -type Item from the player's inventory. */
  def equip: String = {
    val latestEquippable = this.boughtEquippables.filter(this.playerInventory.contains(_)).lastOption
    if (latestEquippable.isDefined) {
      this.equip(latestEquippable.getOrElse("THIS SHOULD NOT APPEAR"))
    } else {
      "You don't have any equippables in your inventory."
    }
  }


  /** Equips all equippables from the player's inventory. */
  def equipall = {
    val filtered = this.playerInventory.filter(n => n._2.typeOfItem == "Weapon" || n._2.typeOfItem == "Armor")
    if (filtered.nonEmpty) {
      this.playerInventory --= filtered.keys
      val (filteredWeapons, filteredArmor) = filtered.partition(_._2.typeOfItem == "Weapon")
      this.equippedWeapons ++= filteredWeapons.map(n => n._1 -> n._2.asInstanceOf[Weapon])
      this.equippedArmor ++= filteredArmor.map(n => n._1 -> n._2.asInstanceOf[Armor])
      s"You equipped ${filtered.values.map(_.name).mkString(", ")}."
    } else {
      "You have nothing to equip."
    }
  }


  /** Unequips an equipped Item and adds it to the player's inventory. */
  def unequip(itemName: String): String = {
    val actualItemName = itemName.toLowerCase
    if (this.equippedWeapons.contains(actualItemName) || this.equippedArmor.contains(actualItemName)) {
      val item = (this.equippedArmor ++ this.equippedWeapons)(actualItemName)
      item match {
        case x if item.typeOfItem == "Weapon" => this.equippedWeapons -= actualItemName
        case x if item.typeOfItem == "Armor" => this.equippedArmor -= actualItemName
        case _ =>
      }
      this.playerInventory += actualItemName -> item
      s"You unequipped ${item.name}."
    } else {
      "You can only unequip equipped weapons and armor."
    }
  }


  /** Uses an Item of type [Consumable / Buff] in the player's inventory. */
  def use(itemName: String): String = {
    val actualItemName = itemName.toLowerCase
    if (this.playerInventory.get(actualItemName).map(_.typeOfItem == "Consumable") == Some(true)) {
      val item = this.playerInventory(actualItemName).asInstanceOf[Consumable]
      val before = this.currentHealthPoints
      this.currentHealthPoints = min(this.currentHealthPoints + item.amount, this.maxHealthPoints)
      this.playerInventory -= actualItemName
      s"You used ${item.name}. It healed you for ${((this.currentHealthPoints - before) * 10).floor / 10.0}"
    } else if (this.playerInventory.get(actualItemName).map(_.typeOfItem == "Buff") == Some(true)) {
      val item = this.playerInventory(actualItemName).asInstanceOf[Buff]
      this.playerInventory -= actualItemName
      item.buff match {
        case "ATTACK"   =>        val before = (this.baseAttack, this.basePhysicalAttack, this.baseMagicAttack)
                                  this.baseAttack += Random.nextInt(item.amount)
                                  this.basePhysicalAttack += Random.nextInt(item.amount)
                                  this.baseMagicAttack += Random.nextInt(item.amount)
                                  s"You used ${item.name}. " +
                                  s"ATTACK buffed by ${this.baseAttack - before._1} | ${this.basePhysicalAttack - before._2} | ${this.baseMagicAttack - before._3} (basic | physical | magic)"

        case "DEFENCE"  =>        val before = (this.baseDefence, this.basePhysicalDefence, this.baseMagicDefence)
                                  this.baseDefence += Random.nextInt(item.amount)
                                  this.basePhysicalDefence += Random.nextInt(item.amount)
                                  this.baseMagicDefence += Random.nextInt(item.amount)
                                  s"You used ${item.name}. " +
                                  s"DEFENCE buffed by ${this.baseDefence - before._1} | ${this.basePhysicalDefence - before._2} | ${this.baseMagicDefence - before._3} (basic | physical | magic)"

        case "HP"       =>        this.maxHealthPoints += item.amount
                                  s"You used ${item.name}. HP buffed by ${item.amount}"

        case "LUCK"     =>        this.luck += item.amount
                                  s"You used ${item.name}. LUCK buffed by ${item.amount}"

        case "ALL"      =>        val statsbefore = Vector(this.maxHealthPoints, this.luck, this.baseAttack, this.basePhysicalAttack, this.baseMagicAttack, this.baseDefence, this.basePhysicalDefence, this.baseMagicDefence)
                                  this.baseAttack += Random.nextInt(item.amount)
                                  this.basePhysicalAttack += Random.nextInt(item.amount)
                                  this.baseMagicAttack += Random.nextInt(item.amount)
                                  this.baseDefence += Random.nextInt(item.amount)
                                  this.basePhysicalDefence += Random.nextInt(item.amount)
                                  this.baseMagicDefence += Random.nextInt(item.amount)
                                  this.maxHealthPoints += Random.nextInt(item.amount * 15)
                                  this.luck += Random.nextInt(item.amount * 5)
                                  val statschange1 = Vector(this.maxHealthPoints, this.luck, this.baseAttack, this.basePhysicalAttack, this.baseMagicAttack, this.baseDefence, this.basePhysicalDefence, this.baseMagicDefence).zip(statsbefore)
                                  val statschange2 = statschange1.map(n => abs(n._1 - n._2))
                                  s"You used ${item.name}. All stats buffed correspondingly: " +
                                  s"\nHP: ${statschange2(0)} | LUCK: ${statschange2(1)}" +
                                  s"\nBASICATK: ${statschange2(2)} | PHYSATK: ${statschange2(3)} | MAGICATK: ${statschange2(4)}" +
                                  s"\nBASICDEF: ${statschange2(5)} | PHYSDEF: ${statschange2(6)} | MAGICDEF: ${statschange2(7)}"

        case _           =>       "THIS SHOULD NOT APPEAR"
      }
    } else {
      s"You don't have ${itemName}."
    }
  }


  /** Uses the last Consumable or Buff from the player's inventory. */
  def use: String = {
    val latestUsable = this.boughtUsables.filter(this.playerInventory.contains(_)).lastOption
    if (latestUsable.isDefined) {
      this.use(latestUsable.getOrElse("THIS SHOULD NOT APPEAR"))
    } else {
      "You don't have any usables in your inventory."
    }
  }


  /** Uses all buffs from the player's inventory. */
  def useall = {
    val filtered = this.playerInventory.filter(_._2.typeOfItem == "Buff")
    if (filtered.nonEmpty) {
      val statsbefore = Vector(this.maxHealthPoints, this.luck, this.baseAttack, this.basePhysicalAttack, this.baseMagicAttack, this.baseDefence, this.basePhysicalDefence, this.baseMagicDefence)
      filtered.keys.foreach(this.use)
      val statschange1 = Vector(this.maxHealthPoints, this.luck, this.baseAttack, this.basePhysicalAttack, this.baseMagicAttack, this.baseDefence, this.basePhysicalDefence, this.baseMagicDefence).zip(statsbefore)
      val statschange2 = statschange1.map(n => abs(n._1 - n._2))
      s"You used all buffs. Stats buffed correspondingly: " +
        s"\nHP: ${statschange2(0)} | LUCK: ${statschange2(1)}" +
        s"\nBASICATK: ${statschange2(2)} | PHYSATK: ${statschange2(3)} | MAGICATK: ${statschange2(4)}" +
        s"\nBASICDEF: ${statschange2(5)} | PHYSDEF: ${statschange2(6)} | MAGICDEF: ${statschange2(7)}"
    } else {
      "You have no buffs."
    }
  }


  /** Returns the description of an item from the player's inventory or equipped items. */
  def examine(itemName: String) = {
    val actualItemName = itemName.toLowerCase
    if (this.has(itemName)) {
      val allItems = this.playerInventory ++ this.equippedWeapons ++ this.equippedArmor
      val item = allItems(actualItemName)
      s"You look closely at the ${item.name}.\n${item.desc}"
    } else  {
      s"You don't have ${itemName}."
    }
  }


  /** Helper method that determines whether the player is at a chop or not. */
  def isAtShop = this.currentLocation.shop.isDefined


  /** Helper method that returns the current shop in an option form. */
  private def currentShop = this.currentLocation.shop


  /** Returns a description of the stats of an item from the player's inventory. */
  def check(itemName: String) = {
    val actualItemName = itemName.toLowerCase
    if (this.has(itemName)) {
      val allItems = this.playerInventory ++ this.equippedWeapons ++ this.equippedArmor
      val item = allItems(actualItemName)
      s"You consult the Gods for the stats of ${item.name}: \n${item.description}"
    } else if (this.isAtShop && this.currentShop.get.contains(itemName)){
      val item = this.currentShop.get.getInventory(actualItemName)
      s"You trust the shopkeeper(?) on the stats of ${item.name}: \n${item.description}"
    } else {
      "You don't have that."
    }
  }


  /** Buys an item from the current shop, if it is defined. */
  def buy(itemName: String) = {
    val actualItemName = itemName.toLowerCase
    if (this.currentShop.map(_.contains(itemName)) == Some(true) && this.wallet >= this.currentShop.map(_.priceOf(itemName)).get) {
      this.wallet -= this.currentLocation.shop.get.getInventory(actualItemName).price
      val removed = this.currentLocation.shop.get.removeItem(itemName)
      this.playerInventory += actualItemName -> removed
      if (removed.typeOfItem == "Weapon" || removed.typeOfItem == "Armor") {
        this.boughtEquippables = this.boughtEquippables :+ actualItemName
      } else {
        this.boughtUsables = this.boughtUsables :+ actualItemName
      }
      s"You bought the ${removed.name}."
    } else if (this.currentShop.map(_.contains(itemName)) == Some(true) && this.wallet < this.currentShop.map(_.priceOf(itemName)).get) {
      s"You don't have enough money for that."
    } else if (this.currentShop.isDefined){
      s"There is no ${itemName} to buy."
    } else {
      "You can only buy at a shop."
    }
  }


  def buyall = {
    val totalPrice: Option[Int] = this.currentShop.map(n => n.getInventory.map(_._2.price).sum)
    if (this.currentShop.map(_.getInventory.isEmpty) == Some(true)) {
      "The shop is empty."
    } else if (totalPrice.map(_ <= this.wallet) == Some(true)) {
      this.wallet -= totalPrice.get
      this.currentShop.map(_.getInventory).foreach(_.foreach(n => this.buy(n._1)))
      "You bought all items from the shop."
    } else {
      "You don't have enough money for that"
    }
  }


  /** Attempts to remove an item from the player's inventory. If that is successful, adds gold equivalent to half of its price to the player's wallet. */
  def sell(itemName: String) = {
    val actualItemName = itemName.toLowerCase
    if (this.has(itemName)) {
      val item = (this.playerInventory ++ this.equippedWeapons ++ this.equippedArmor)(actualItemName)
      val amount = item.price * 0.5
      this.wallet += amount.ceil.toInt
      this.playerInventory -= actualItemName
      this.equippedWeapons -= actualItemName
      this.equippedArmor -= actualItemName
      s"You sold ${item.name} for ${amount.ceil.toInt}G."
    } else {
      "You don't have that!"
    }
  }


  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name


}
