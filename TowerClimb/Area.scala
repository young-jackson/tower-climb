package TowerClimb

import scala.collection.mutable.Map
import scala.util.Random


/** The class Area has a few variables indicating its floor (1 to 5), reference to
  * a shop if it has one, and a boolean of whether the area is an arena or not. */

class Area(val floor: Int, val shop: Option[Shop], val arena: Boolean) {


  /** Indicates whether there is a combat going on in the current arena. */
  var combat: Option[Combat] = None


  /** Indicates the neighbors of the area. */
  protected val neighbors = Map[String, Area]()


  /** Initiates combat in this area, if there are any monsters left to fight on this floor. */
  def initiateCombat(player: Player): String = {
    if (combat.map(_.outOfMonsters) != Some(true)) {
      combat = Some(new Combat(player, floor))
    }
    if (this.combat.get.monster.isDefined) {
      "Seemingly out of nowhere, you see an enemy appear."
    } else {
      "You have defeated all monsters on this floor."
    }
  }


  /** Returns the type of area based on the variables 'arena' and 'shop'. */
  def returnModifiers = {
    if (shop.isDefined) {
      "Shop"
    } else if (arena) {
      "Arena"
    } else if (this.neighbors.contains("up")) {
      "Ladder Room"
    } else {
      "Main Hall"
    }
  }


  /** Returns the full name of the area. */
  def name = floor match {
    case 0 => "Tower Entrance"
    case x if x <= 4 => s"Floor $x " + returnModifiers
    case _ => "Top Floor"
  }


  /** Defines the description of the area. */
  val description = floor match {
    case 0 => "Staring at the tower, you feel a dreadful sense of excitement. What could lie behind those ragged bricks? Only one way to find out."
    case 1 => "Ground floor... Nothing much to see here i guess."
    case 2 => "Looks the same as the previous floor. At least there are some new enemies to see."
    case 3 => "There seems to be some difficult-to-deal-with folks here now."
    case 4 => "The view from up here is amazing!"
    case 5 => "This is it. Youre at the top of the tower, and in front you see the silhouette of a demonic creature. You look around you, and all you can see is stormy clouds and distant mountain tops. " +
      "\nThe sky is roaring and the raindrops feel like golf balls splattering on your grimy figure. Only one command between you and glory."
    case _ => "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  }


  /** Returns the area that can be reached from this area by moving in a direction in option form. */
  def neighbor(direction: String) = this.neighbors.get(direction)


  /** Adds an exit from this area to another area. */
  def setNeighbor(direction: String, neighbor: Area) = {
    this.neighbors += direction -> neighbor
  }


  /** Adds exits from this area to multiple areas. */
  def setNeighbors(exits: Vector[(String, Area)]) = {
    this.neighbors ++= exits
  }


  /** An elaborate method for determining the state of the area, ie. the full description of the area. */
  def fullDescription = {
    val exitList = "\n\nDirections available: " + this.neighbors.keys.mkString(" ")
    val items = if (this.shop.isDefined && !(this.shop.map(_.isEmpty) == Some(true))) {       // Creates a categorized description of the items that the Option[Shop] offers.
      def ordering(typeOfItem: Char) = {
        typeOfItem match {
          case 'W' => 0
          case 'A' => 1
          case 'C' => 2
          case 'B' => 3
          case _ => 4
        }
      }
      val shopItems1 = this.shop.get.getInventory.toVector.map(_._2)
      val shopItems2 = shopItems1.groupBy(_.typeOfItem)
      val shopItems3 = shopItems2.map(n => (if (n._1 != "Armor") n._1 + "s: " +  " " * (11 - n._1.length)  else n._1 + ": " + " " * (12 - n._1.length)) -> n._2).toVector.sortBy(n => ordering(n._1.head))
      val shopItems4 = shopItems3.map(n => n._1 + n._2.sortBy(_.price).mkString(", "))
      s"The shop seems to be offering many items: \n${shopItems4.mkString("\n")}"          // Return value of variable if this is a shop with items. (1)
    } else
      "The shop is empty."                                                                 // Return value of variable if the shop is empty. (2)
    if (this.floor == 0) {
      this.description + exitList                                                          // If this is the starting area.
    } else if (this.arena && this.combat.isEmpty) {
      val extra = if (this.floor == 1) "\nNote that only after having defeated a monster are you able to proceed past the arena." else ""
      Combat.battlearena(this.floor) + "\nStart a battle with 'fight'." + extra + exitList // If this is an arena and does not have combat.
    } else if (this.combat.isDefined && this.combat.get.isOver) {
      this.combat.get.info + exitList                                                      // If this is an arena and has a completed combat.
    } else if (this.combat.isDefined && !this.combat.get.isOver) {
      this.combat.get.info + "\n\nNo running away from combat."                            // If this is an arena and has an ended combat.
    } else if (this.shop.isDefined) {
      items + exitList                                                                     // If this is a shop that is non-empty (1), or empty (2).
    } else if (this.neighbors.contains("up") && this.floor == 4) {
      "You're by the ladder to the top floor. You sense a malicious presence above you." + exitList                 // If this is the ladder room of the 4th floor.
    } else if (this.neighbors.contains("up")) {
      "You're by the ladder to the next floor. It is your decision whether you want to ascend yet." + exitList      // If this is any other ladder room.
    } else {
      this.description + exitList                                                          // If this is a Main Hall.
    }
  }


  def isLadder = this.neighbors.contains("up")


  /** Returns a boolean of whether the ladder room is unlocked, ie. whether a monster has been defeated on this floor. */
  def ladderunlocked = Combat.floorInstanceNumbers(this.floor - 1) != 0


  /** Returns a single-line description of the area for debugging purposes. */
  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)


}


/** The class Shop has a single instance variable indicating the floor. */

class Shop(floor: Int) {

  import Shop._


  /** Indicates the map of items that the shop has in store. */
  private val inventory = Map[String, Item]()


  /** Adds items to the inventory accodring to which floor it is. */
  floor match {
    case 1 => this.addItem(offeredWeapons.take(3))
              this.addItem(offeredArmor.take(3))
              this.addItem(offeredConsumables.take(3))
              this.addItem(offeredBuffs.take(2))
    case 2 => this.addItem(offeredWeapons.slice(3, 6))
              this.addItem(offeredArmor.slice(3, 6))
              this.addItem(offeredConsumables.slice(3, 6))
              this.addItem(offeredBuffs.slice(2, 4))
    case 3 => this.addItem(offeredWeapons.slice(6, 9))
              this.addItem(offeredArmor.slice(6, 9))
              this.addItem(offeredConsumables.slice(6, 11))
              this.addItem(offeredBuffs.slice(4, 6))
    case 4 => this.addItem(offeredLegendaryItems)
              this.addItem(offeredWeapons.slice(9, 11))
              this.addItem(offeredArmor.slice(9, 11))
              this.addItem(offeredConsumables.slice(11, 12))
              this.addItem(offeredBuffs.slice(6, 9))
  }


  /** Returns a boolean for whether the shop is empty. */
  def isEmpty = this.inventory.isEmpty


  /** Public method for a private variable. */
  def getInventory = this.inventory


  /** Returns a boolean for whether the shop contains a certain item. */
  def contains(itemName: String) = this.inventory.contains(itemName.toLowerCase)


  /** Returns the price of a certain item in the shop. If there is no such item, returns 999999. */
  def priceOf(itemName: String) = this.inventory.get(itemName.toLowerCase).map(_.price).getOrElse(999999)


  /** Adds an item to the inventory of the shop. */
  def addItem(item: Item) = {
    this.inventory += item.name.toLowerCase -> item
  }


  /** Adds items to the inventory of the shop. */
  def addItem(items: Vector[Item]) = {
    val itemsMap = items.map(_.name.toLowerCase).zip(items)
    this.inventory ++= itemsMap
  }


  /** USE THE FOLLOWING METHOD ONLY WHEN THE SHOP HAS THE ITEM
    * Removes an item from the shop. Returns the removed item. */
  def removeItem(itemName: String) = {
    val result = this.inventory(itemName.toLowerCase)
    this.inventory -= itemName
    result
  }

}


/** Helper class with the preset items. */

object Shop {


  /** Weapons: */
  val Sword     = new Weapon("Polished Dagger", "Looks like an ordinary dagger.",                                                      50,  None,             10  )
  val Wand      = new Weapon("Fancier Wand", "I could replace my wand with this one, or use them both at the same time!",              50,  Some("magic"),    15  )
  val Mace      = new Weapon("Spiked Mace", "Feels pretty heavy, but at least it's effective.",                                        100, Some("physical"), 20  )
  val Ring      = new Weapon("Magic Ring", "One ring for domination!! ...is how it went right?",                                       100, Some("magic"),    20  )
  val Knuckles  = new Weapon("Bass Knuckles", "Is this a typo? No right? Theyre clearly made out of fish...",                          150, Some("physical"), 30  )
  val RatStick  = new Weapon("Rat on a Stick", "Who would even use this? Is smells disgusting.",                                       200, None,             45  )
  val Cheese    = new Weapon("Cheese Gun", "Interesting design on this one...",                                                        250, Some("physical"), 60  )
  val Staff     = new Weapon("Water staff", "Looks like a real weapon!",                                                               300, Some("magic"),    80  )
  val Torch     = new Weapon("Glass Torch", "These exist? Looks pretty powerful... and magical.",                                      350, Some("magic"),    100 )
  val Frisbee   = new Weapon("Sharp Frisbee", "Almost cut myself just by holding it. Huh.",                                            450, None,             150 )
  val Rod       = new Weapon("Large Rod", "Seems to have some magical properties. Can't figure out for sure though.",                  550, Some("magic"),    150 )
  val Bsword    = new Weapon("Adamantite Broadsword", "Is this excalibur?",                                                            700, Some("physical"), 300 )
  val Raygun    = new Weapon("Ray Gun", "Must have gotten good luck.",                                                                 800, None            , 400 )


  /** Armor: */
  val Chainmail = new Armor("Chainmail", "Looks like something from the medieval times. This will probably give some defence.",        50,  None,             10  )
  val Jeans     = new Armor("Jeans", "Who even wears these? Made out of a mysterial material.",                                        50,  None,             10  )
  val Hat       = new Armor("Magical Hat", "Funny looking hat, probably magical too.",                                                100,  Some("magic"),    15  )
  val Budget    = new Armor("Budget Rolex", "Seemed like a good investment",                                                          150,  Some("physical"), 25  )
  val Bracelets = new Armor("Cool Bracelets", "Ornamental bracelets... wowie pretty shiny.",                                          150,  None,             30  )
  val Rolex     = new Armor("Rolex", "Catches my attention at least... maybe it works similarly on enemies too",                      250,  Some("magic"),    40  )
  val Chain     = new Armor("Gold-plated Cuban Chains", "Drippy, these would make me seem cooler at least.",                          300,  None,             50  )
  val Crystal   = new Armor("Protective Crystal", "This one seems to have a trusty aura around it.",                                  400,  Some("magic"),    90  )
  val Mythril   = new Armor("Mythril Chainvest", "Looks extremely expensive, has some carvings in a foreign lingo. " +
                            "Probably protects me from everything.",                                                                  400,  Some("physical"), 90  )
  val Jacket    = new Armor("Warm Jacket", "What material is this even? At least it keeps me warm.",                                  500,  None,             120 )
  val Kevlar    = new Armor("Kevlar Vest", "I feel immortal in this.",                                                                700,  None,             150 )
  val Robe      = new Armor("Shiny Robe", "Looks like some Wizard's piece of clothing.",                                              650,  Some("magic"),    150 )


  /** 4th Floor legendary Items: */
  val OneRing   = new Weapon("Ring of Power", "the ring is MINE.", 800, Some("magic"), 250)
  val Saber     = new Weapon("Lazer Sword", "Makes a funky noise when I swing it.", 800, Some("physical"), 250)
  val Excalibur = new Weapon("Excalibur", "Someone pulled it out of the stone.", 1200, None, 350)
  val Horcrux   = new Armor("Horcrux", "Seems dangerous but I still feel like it could keep me safe.", 800, Some("physical"), 250)
  val Iron      = new Armor("Iron Plate", "It's just an iron plate with two straps...", 800, Some("physical"), 250)
  val Plot      = new Armor("Plot Armor", "Oh, I guess I'm invincible now.", 1200, None, 350)
  val Karjala   = new Consumable("Karjalan Piirakka", "Delicious Elvish food yum.", 700, 4000)
  val Gifflar   = new Consumable("Gifflar", "Delicious Elvish food yum.", 400, 2000)
  val Kossu     = new Buff("Kossu", "Jep.", 1000, 100, "ALL")
  val Apple     = new Buff("Gapple", "Stands for 'Golden Apple'.", 1000, 2000, "HP")


  /** Groups the aforementioned items by their class types. */
  val offeredWeapons = Random.shuffle(Vector(Sword, Wand, Mace, Ring, Knuckles, RatStick, Cheese, Staff, Torch, Frisbee, Rod, Bsword, Raygun))
  val offeredArmor = Random.shuffle(Vector(Chainmail, Jeans, Hat, Budget, Bracelets, Rolex, Chain, Crystal, Mythril, Jacket, Kevlar, Robe))
  val offeredLegendaryItems = Random.shuffle(Vector(OneRing, Saber, Excalibur, Horcrux, Iron, Plot, Karjala, Gifflar, Kossu, Apple))


  /** Buffs: */
  val Milk      = new Buff("Whole Milk", "Used to be what I would get at a nice pub before I found out about skim milk.",            100, 10, "DEFENCE" )
  val Celery    = new Buff("Celery", "Some people really hate this don't they?",                                                     150,  8, "ALL"     )
  val Clover    = new Buff("Lucky Clover", "These are pretty rare. I think I should eat it.",                                        100, 20, "LUCK"    )
  val Burger    = new Buff("Cheese Burger", "Lovin' it.",                                                                            150, 20, "ATTACK"  )
  val LuckyPot  = new Buff("Potion of Luck", "Shiny looking yellow juice.",                                                          150, 40, "LUCK"    )
  val Quinoa    = new Buff("Quinoa", "Seeds? At least its healthy.",                                                                 150, 40, "DEFENCE" )
  val Spice     = new Buff("Spice", "Strange-looking powder... I could, and probably should try eating it just to see what it is.",  150, 400, "HP"     )
  val Broccoli  = new Buff("Broccoli", "Yim yum veggies.",                                                                           200, 25, "ATTACK"  )
  val Cookie    = new Buff("Fortune Cookie", "Feelin' lucky.",                                                                       200, 80, "LUCK"    )
  val Chia      = new Buff("Pouch of Chia Seeds", "It dun get healthier than this.",                                                 250, 30, "DEFENCE" )
  val Whopper   = new Buff("Whopper", "Yummy burger",                                                                                300, 400, "HP"     )
  val Dragon    = new Buff("Dragonfruit", "Looks kinda funky and magical.",                                                          400, 35, "ATTACK"  )
  val GodFruit  = new Buff("God Fruit", "What divine food is this?",                                                                 800, 30, "ALL"     )


  /** Groups the buffs. */
  val offeredBuffs = Random.shuffle(Vector(Milk, Celery, Clover, Burger, LuckyPot, Quinoa, Spice, Broccoli, Cookie, Chia, Whopper, Dragon, GodFruit))


  /** Consumables: */
  val Potion    = new Consumable("Regular Potion", "Looks healthy.",                            50, 100  )
  val MedPotion = new Consumable("Larger Potion", "Looks healthier.",                          100, 200  )
  val BigPotion = new Consumable("Huge Potion", "Looks the healthiest.",                       250, 600  )
  val Spaghetti = new Consumable("Spaghetti", "From Rome.",                                    150, 200  )
  val Stew      = new Consumable("Mushroom Soup", "From Rome.",                                200, 350  )
  val Pizza     = new Consumable("Pizza Salami", "From New York.",                             250, 450  )
  val Applepie  = new Consumable("Applepie", "Yummy.",                                         175, 350  )
  val Yoghurt   = new Consumable("Yoghurt", "Mmmmm...",                                        200, 500  )
  val Godfood   = new Consumable("Kanelipulla", "What kind of Elvish food is this?",           325, 1000 )
  val Mead      = new Consumable("Whole Chicken", "Now we're talkin'.",                        600, 2000 )
  val Golden    = new Consumable("Golden Steak", "People eat this?",                           800, 2500 )


  /** Groups the consumables. */
  val offeredConsumables = Random.shuffle(Vector(Potion, MedPotion, BigPotion, Spaghetti, Stew, Pizza, Applepie, Yoghurt, Godfood, Mead, Golden))


  /** Returns a list of all items, grouped by their types and sorted by their stats to price ratio. */
  def Tiers() = {
    println("Weapons: ")
    (this.offeredWeapons ++ Vector(OneRing, Saber, Excalibur)).sortBy(n => -n.attackDamage * 1.0 / n.price).map(n => n.name + " | value: " + (((n.attackDamage * 1.0 / n.price) * 100).toInt / 100.0 + " | price: " + n.price + "G")).foreach(println)
    println("\nArmor: ")
    (this.offeredArmor ++ Vector(Horcrux, Iron, Plot)).sortBy(n => -n.defenceAmount * 1.0 / n.price).map(n => n.name + " | value: " + (((n.defenceAmount * 1.0 / n.price) * 100).toInt / 100.0 + " | price: " + n.price + "G")).foreach(println)
    println("\nConsumables: ")
    (this.offeredConsumables ++ Vector(Karjala, Gifflar)).sortBy(n => -n.amount * 1.0 / n.price).map(n => n.name + " | value: " + (((n.amount * 1.0 / n.price) * 100).toInt / 100.0 + " | price: " + n.price + "G")).foreach(println)
    println("\nCOMBAT Buffs: ")
    this.offeredBuffs.filter(n => n.buff == "ATTACK" || n.buff == "DEFENCE").sortBy(n => -n.amount * 1.0 / n.price).map(n => n.name + " | value: " + (((n.amount * 1.0 / n.price) * 100).toInt / 100.0 + " | price: " + n.price + "G")).foreach(println)
    println("\nLUCK Buffs: ")
    this.offeredBuffs.filter(n => n.buff == "LUCK").sortBy(n => -n.amount * 1.0 / n.price).map(n => n.name + " | value: " + (((n.amount * 1.0 / n.price) * 100).toInt / 100.0 + " | price: " + n.price + "G")).foreach(println)
    println("\nHP Buffs: ")
    (this.offeredBuffs :+ Apple).filter(n => n.buff == "HP").sortBy(n => -n.amount * 1.0 / n.price).map(n => n.name + " | value: " + (((n.amount * 1.0 / n.price) * 100).toInt / 100.0 + " | price: " + n.price + "G")).foreach(println)
    println("\nALL Buffs: ")
    (this.offeredBuffs :+ Kossu).filter(n => n.buff == "ALL").sortBy(n => -n.amount * 1.0 / n.price).map(n => n.name + " | value: " + (((n.amount * 1.0 / n.price) * 100).toInt / 100.0 + " | price: " + n.price + "G")).foreach(println)
  }


}

