package TowerClimb

import scala.collection.mutable.Buffer
import scala.math._
import scala.util.Random


/** The class Combat has two instance variables: Floor, and player, which indicates the player
  * that starts a combat. */

class Combat(val player: Player, val floor: Int) {

  import Combat.{AllMonsters, Boss, battlearena, floorInstanceNumbers}


  /** Starting variables. firstTime indicates if it is the first time the player visits the same arena/combat. */
  var monster: Option[Monster] = None
  var firstTime = true


  /** Adds a monster according to the floor number. A different monster is added each time until the floor runs
    * out of monsters, in which case the monster variable remains empty. */
  floor match {
    case x if x <= 3 && floorInstanceNumbers(x - 1) < AllMonsters(x - 1).length => monster = Some(AllMonsters(x - 1)(floorInstanceNumbers(x - 1)))
                                                                                   floorInstanceNumbers(x - 1) += 1

    case 4 if floorInstanceNumbers(3) < AllMonsters(3).length                   => monster = Some(AllMonsters(3)(floorInstanceNumbers(3)))
                                                                                   floorInstanceNumbers(3) += 1
    case 5 => monster = Some(Boss)
    case _ => monster = None
  }


  /** Returns a boolean of whether the combat is over or not. */
  def isOver = monster.map(_.health <= 0) == Some(true) || player.hasDied


  /** Damages both the player and the monster by an amount derived from the opponents effective attack stats. */
  def offensive(x: Int): String = {
    val actor1 = this.player.takeDamage(monster.get, x)
    val actor2 = monster.get.takeDamage(player, x)
    if (this.monster.map(_.health <= 0) == Some(true)) {
      this.player.wallet += this.monster.map(n => (n.loot * this.player.getLuck / 100.0).floor.toInt).getOrElse(0)
    }
    actor1 + "\n" + actor2
  }


  /** Returns a description of the monster's health bar in in text form if the player is in combat.
    * Otherwise returs a description of the loot collected by defeating the monster.
    * In the case where there are no monsters left it returns an other message.*/
  def info: String = {
    if (this.monster.map(_.isAlive) == Some(true)) {
      if (this.monster == Some(Boss)) {
        val initialprompt = if (Boss.health > 5000) "We finally meet!" else "What a horribly evil looking creature! How does he have so much health? Better strike him with all my might! "
        initialprompt + s"\n${this.monster.get.name}: " + "▓" * (this.monster.get.health / 10.0).ceil.toInt + "░" * ((this.monster.get.maxHealthPoints - this.monster.get.health) / 10.0).floor.toInt
      } else {
        s"You face ${this.monster.get.name}!\nEnemy HP: " + "▓" * (this.monster.get.health / 10.0).ceil.toInt + "░" * ((this.monster.get.maxHealthPoints - this.monster.get.health) / 10.0).floor.toInt
      }
    } else if (this.monster.map(_.isAlive) == Some(false)) {
      val text = if (firstTime) {
        firstTime = false
        s"You have defeated ${this.monster.get.name}! Loot collected: ${(this.monster.get.loot * this.player.getLuck / 100.0).floor.toInt}G"
      } else battlearena(this.floor)
      val extratext = if (this.outOfMonsters) "\nYou have defeated all monsters on this floor." else "\nChallenge another with 'fight'."
      text + extratext
    } else {
      "THIS SHOULD NOT APPEAR"
    }
  }


  /** Returns a boolean of whether this floor is out of monsters. */
  def outOfMonsters = floorInstanceNumbers(this.floor - 1) >= Combat.AllMonsters(this.floor - 1).length


}


/** Helper class that contains all of the defined monsters and assigns them for each floor. */

object Combat {


  /** Debug purposes: */
  override def toString: String = s"${floorInstanceNumbers}"


  /** Shows the amount of instances of new combats on each floor. */
  var floorInstanceNumbers = Buffer(0, 0, 0, 0)


  /** Floor 1 monsters: */
  val Newbie    = new Monster("Newbie Orc", "Some rando noob orco...",                                                100, 5, 0, 20, 10, false,   100  )
  val Welp      = new Monster("Tiny Welp", "Who's this small creature?",                                              150, 0, 5, 10, 20, false,   120 )
  val Goblin    = new Monster("Green Goblino", "I wouldn't trust this guy...",                                        180, 10, 10, 10, 10, false, 210 )
  val Minion    = new Monster("Yellow Minion", "Why is HE here?",                                                     100, 0, 0, 20, 20, false,   170 )
  val Crab      = new Monster("Sand Crab", "Better farm these for a lot of XP.",                                      300, 0, 0, 5, 5, false,     100  )
  val Ghoul     = new Monster("Ghoul", "Thats a ghoul...",                                                            200, 0, 0, 10, 10, false,   220 )


  /** Floor 2 monsters: */
  val Yeti      = new Monster("Ice Yeti", "Still seems way smaller than one would imagine... for a yeti at least.",   175, 15, 0, 40, 0, false,   100 )
  val Cleric    = new Monster("Corrupt Cleric", "Somehow I can't believe that its a human by its appearance.",        200, 0, 15, 10, 30, false,  250 )
  val Goose     = new Monster("Goose", "How did this guy end up here?",                                               300, 15, 15, 20, 20, false, 300 )
  val Dog       = new Monster("Terrordog", "Someone wanted me to do slayer tasks?",                                   350, 10, 10, 40, 20, false, 200 )
  val Mole      = new Monster("Large Mole", "Feeling like I could use me some slippers.",                             400, 40, 40, 10, 10, false, 300 )
  val Mugger1   = new Monster("Deprived Soul", "Maybe I was being a bit too greedy...",                               700, 15, 15, 25, 25, false, 0   )
  val Mugger2   = new Monster("Soulless Wanderer", "Maybe I was being a bit too greedy...",                           800,  0,  0, 30, 30, false, 0   )
  val Knight1   = new Monster("Divine Knight", "Finally a challenge...",                                              800,  0, 70, 0, 100, false, 550 )
  val Knight2   = new Monster("Demon King", "Finally a challenge...",                                                 800, 70,  0, 100, 0, false, 550 )


  /** Floor 3 monsters: */
  val Troll     = new Monster("Cave Troll", "What is that smell?",                                                    300, 50, 40, 60, 20, false, 350 )
  val Sentinel  = new Monster("Mana Golem", "...",                                                                    300, 40, 50, 20, 60, false, 400 )
  val Gobo      = new Monster("Resurrected Goblino", "I swear I've seen this guy somewhere before.",                  400, 40, 40, 60, 60, false, 400 )
  val Tortoise  = new Monster("Warped Tortoise", "'Turtle from the poison lake' indeed.",                             600, 70, 70, 20, 20, false, 300 )
  val Mugger3   = new Monster("Image of the Abyss", "Maybe I was being a bit too greedy...",                          1200, 30, 30, 30, 30, false, 0  )
  val Mugger4   = new Monster("Shadow Colossus", "Maybe I was being a bit too greedy...",                             1400,  0,  0, 40, 40, false, 0  )
  val Knight3   = new Monster("The Witch King", "Finally a challenge...",                                             1800, 0, 120, 0, 150, false, 650 )
  val Knight4   = new Monster("Aspect of Wrath", "Finally a challenge...",                                            1800, 120, 0, 150, 0, false, 650 )


  /** Floor 4 monsters: */
  val Cyclops   = new Monster("Two-eyed Cyclops", "It's a giant.",                                                    700, 180, 0, 160, 20, false,  450 )
  val Wizard    = new Monster("Evil Wizard", "He doesn't even look that menacing. All he has is an evil hat...",      700, 10, 150, 50, 100, false, 500 )
  val Lord      = new Monster("Flying Octopus", "Wtf is that AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",                     900, 100, 100, 90, 90, false, 700 )
  val Mugger5   = new Monster("Neverending Nightmare", "Maybe I was being a bit too greedy...",                    2000, 100, 100, 100, 100, false, 0   )
  val Knight5   = new Monster("The Devourer of Gods", "Finally a challenge...",                                   2000, 300, 300, 200, 200, false, 1000 )


  /** Assigning monsters to their floors: */
  val Monsters1 = Random.shuffle(Vector(Newbie, Welp, Goblin, Minion, Crab, Ghoul))
  var Monsters2 = Random.shuffle(Vector(Yeti, Cleric, Goose, Dog, Mole))
  var Monsters3 = Random.shuffle(Vector(Troll, Sentinel, Gobo, Tortoise))
  var Monsters4 = Random.shuffle(Vector(Cyclops, Wizard, Lord))


  /** Defines the boss of the game. */
  var Boss = new Monster("John the Rat King", "Wow what a funky boss.", 4000, 400, 400, 200, 200, true, 10000 )
  val Smith = new Monster("William, the Blacksmith of War", "What? I thought he was casting for his new movie???", 6000, 700, 700, 600, 600, true, 10000)


  /** All monsters grouped into a single 2D vector. */
  def AllMonsters = Vector(Monsters1, Monsters2, Monsters3, Monsters4)


  /** Helper method that returns the description of the arena of each floor. */
  def battlearena(floor: Int) = floor match {
    case 1 => "The arena looks plain, and a little filthy. The walls are stained with god-knows-what."
    case 2 => "This looks better than the first floor, at least there are some monsters watching behind the barbed fences."
    case 3 => "The height of the ceiling makes the arena seem three-folds the size of the previous one. The walls and the groundwork seem reinforced and way sturdier. Seems like it's designed to fit larger creatures."
    case 4 => "The size of this arena is even larger, but it's probably for the safety of the other monsters. There is an awfully thick wall, and the foundation is made out of adamantium."
    case _ => ""
  }

}


/** The class Monster has a lot of instance variables that each correspond to a stat that the created monster will have. */

class Monster(val name: String, val description: String, var health: Double, var armorPhysical: Int, var armorMagic: Int, var attackPhysical: Int, var attackMagic: Int, val boss: Boolean, val loot: Int) {


  /** Defines the max health of the monster in a non-conventional way. */
  val maxHealthPoints = health


  /** List of helpermethods that define how the monster takes damage. */
  private def takeDamagePhysical(amount: Double) = this.health -= amount * (1 - this.armorPhysical * 1.0 / (100 + this.armorPhysical))
  private def takeDamageMagic(amount: Double) = this.health -= amount * (1 - this.armorMagic * 1.0 / (100 + this.armorPhysical))
  private def takeDamageBasic(amount: Double) = if (boss) this.health -= amount else this.health -= amount * (1 - (this.armorPhysical + this.armorMagic) * 1.0 / (100 + this.armorMagic + this.armorPhysical))


  /** Defines how the monster takes damage from a player x times. Returns the HP value after the attack. */
  def takeDamage(player: Player, x: Int) = {
    def round(x: Double) = (x * 10).ceil / 10.0
    val before = this.health
    this.takeDamagePhysical(x * player.attackPhysical)
    this.takeDamageMagic(x * player.attackMagic)
    this.takeDamageBasic(x * player.attackBasic)
    if (this.health < 0) this.health = 0
    s"${this.name} HP: [${round(this.health)} / ${this.maxHealthPoints}], dealt ${round(before - this.health)}"
  }


  /** Returns a boolean indicating whether the monster is alive or not. */
  def isAlive = this.health > 0


}
