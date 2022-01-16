package TowerClimb

import Combat._

import scala.util.Random

class Action(input: String) {


  /** Concerns the player's inputs. */
  private val commandText = input.trim
  private val verb        = commandText.takeWhile( _ != ' ' ).toLowerCase
  private val modifiers   = commandText.drop(verb.length).trim


  /** Causes the given player to take the action represented by this object, assuming
    * that the command was understood. Returns a description of what happened as a result
    * of the action. The description is returned in an `Option` wrapper; if the command was
    * not recognized, `None` is returned. */
  def execute(actor: Player) = {
    this.verb match {
      case "go"        => if (actor.location.isLadder && this.modifiers == "up") {
                            var extra = ""
                            actor.location.floor match {
                              case 1 => if (actor.greed >= 200) {
                                          Monsters2 = Random.shuffle(Monsters2 ++ Vector(Mugger1, Mugger2))
                                          extra += "\nYou suddenly feel as if your greed is going to haunt you."
                                        }
                                        if (actor.statSum >= 200) {
                                          Monsters2 = Random.shuffle(Monsters2 ++ Vector(Knight1, Knight2))
                                          extra += "\nYou feel a presense arriving at the floor from lands far away."
                                        }
                              case 2 => if (actor.greed >= 300) {
                                          Monsters3 = Random.shuffle(Monsters3 ++ Vector(Mugger3, Mugger4))
                                          extra += "\nYou suddenly feel as if your greed is going to haunt you."
                                        }
                                        if (actor.statSum >= 500) {
                                          Monsters3 = Random.shuffle(Monsters3 ++ Vector(Knight3, Knight4))
                                          extra += "\nYou feel a presense arriving at the floor from lands far away."
                                        }
                              case 3 => if (actor.greed >= 400) {
                                          Monsters4 = Random.shuffle(Monsters4 :+ Mugger5)
                                          extra += "\nYou suddenly feel as if your greed is going to haunt you."
                                        }
                                        if (actor.statSum >= 800) {
                                          Monsters4 = Random.shuffle(Monsters4 :+ Knight5)
                                          extra += "\nYou feel a presense arriving at the floor from lands far away."
                                        }
                              case 4 => if (actor.statSum >= 3000) {
                                          Boss = Smith
                                          extra += "\nSomething doesn't feel right..."
                                        }
                              case _ =>
                            }
                            Some(actor.go("up") + extra)
                          } else {
                            if (actor.isInCombat) Some("You can't run away from combat.") else Some(actor.go(this.modifiers))
                          }
      case "fight"     => if (actor.isInCombat) Some("You're already fighting.") else Some(actor.fight)
      case "attack" if this.modifiers.toIntOption.isDefined  => if (actor.isInCombat) Some(actor.location.combat.get.offensive(this.modifiers.toInt)) else Some("You are not in combat.")
      case "attack"    => if (actor.isInCombat) Some(actor.location.combat.get.offensive(1)) else Some("You are not in combat.")
      case "quit"      => Some(actor.quit())
      case "inventory" => Some(actor.inventory)
      case "buy"       => if (actor.isInCombat) Some("You're in combat, not in a shop.") else { if (this.modifiers == "all") Some(actor.buyall) else Some(actor.buy(this.modifiers))}
      case "equip" if this.modifiers == "all" => Some(actor.equipall)
      case "equip" if this.modifiers == ""    => Some(actor.equip)
      case "equip"     => Some(actor.equip(this.modifiers))
      case "unequip"   => Some(actor.unequip(this.modifiers))
      case "examine" if this.modifiers == "" && actor.isInCombat => Some(actor.examine)
      case "examine"   => Some(actor.examine(this.modifiers))
      case "stats"     => Some(actor.stats)
      case "statcheck" => Some(actor.statcheck)
      case "check"     => Some(actor.check(this.modifiers))
      case "use" if this.modifiers == "all"  => Some(actor.useall)
      case "use" if this.modifiers == ""     => Some(actor.use)
      case "use"       => Some(actor.use(this.modifiers))
      case "equipped"  => Some(actor.equipped)
      case "help"      => Some(this.help)
      case "sell"      => Some(actor.sell(this.modifiers))
      case "/dev"       => Some(actor.dev)
      case "/addattack" if this.modifiers.toIntOption.isDefined => Some(actor.addAttack(this.modifiers.toInt))
      case "/adddefence" if this.modifiers.toIntOption.isDefined => Some(actor.addDefence(this.modifiers.toInt))
      case "/addgold" if this.modifiers.toIntOption.isDefined => Some(actor.addGold(this.modifiers.toInt))
      case "/heal"     => Some(actor.heal)
      case other       => None
    }
  }


  /** Prints the description of the game in addition to the commands. */
  def help: String = {
      "\n=========================================================================================================================================================" +
      "\nThe game is designed to be similar to certain roguelike games. The objective is to complete each level (floors 1 to 4) by defeating at least one" +
      "\nmonster in the arena. This unlocks the way to the ladder room of the current floor, through which you can progress onto the next floor. Each floor" +
      "\nhas a shop, where you can purchase items from. These items are divided into categories [Weapon], [Armor], [Consumable], and [Buff]." +
      "\nFinally, at the top (5th) floor you fight the final boss, defeating which wins the game. The roguelike part of this game comes from the element of" +
      "\nrandomness in the enemies that you fight in each arena and from the cycle of items that are offered in the shops, which makes sure that each playthrough" +
      "\nis different from each other. Note that buffs are also randomized in how they add to each of the player's stats. The player is allowed to equip as many" +
      "\nitems as possible, which fits into the nature of many roguelike games." +
      "\n\nThe main aspect of the game, ie. the combat system, relies on three different types of attacks: basic, physical, and magic. Similarly, there are three" +
      "\ntypes of defences corresponding to each type (practically each basic stat corresponds to physical and magic equally, making it only two types of attacks)." +
      "\nThese make up the first 6 stats that the player has out of the total of 8 different buffable stats available. The last two are HP and LUCK, with the former" +
      "\none corresponding to your maximum hitpoints and the latter indicating the procentage of extra gold acquired after each battle." +
      "\n\nThe types of items that can be acquired from the shop increase your stats accordingly:" +
      "\nWeapons increase their corresponding attack type as armor does with the defence types. Consumables increase your current health, and buffs increase all" +
      "\nstats according to their specifications. To elaborate, ATTACK and DEFENCE buffs increase each of their three stats by a random amount from 0 to the amount" +
      "\nspecified by the buff. LUCK and (max) HP buffs increase the stat by a flat, non-randomized amount. Note that for ALL buffs, HP buffs are randomized too" +
      "\nand the LUCK stat is buffed by 5 times its amount." +
      "\nThere are additionally the stats POWER LVL and GREED, which correspond to the weighed sum of all stats, and the sum of your gold and inventory price," +
      "\nrespectively. It is important to note that during a floor transition from 1 -> 2, 2 -> 3, or 3 -> 4, the game checks these stats, and changes the difficulty" +
      "\nby adding enemies accordingly, so beware of that, too." +
      "\n\nItems can be bought from the shop for a flat amount of gold. Each item can be inspected and examined for their stats. Weapons and armor bought from the" +
      "\nshop need to be equipped and buffs need to be used before they're effective. This can be done regardless of whether you're in combat or not." +
      "\n=========================================================================================================================================================" +
      "\n\nThe Commands are described below:\n" +
      "\ngo        + direction            moves the player accordingly" +
      "\ninventory                        displays what is in the player's inventory" +
      "\nquit                             quits the game" +
      "\nequipped                         displays what gear the player has equipped (items are mutually exclusive with those in the inventory)" +
      "\nfight                            starts a battle if the player is at an arena and there are monsters left" +
      "\nexamine                          describes the monster [COMBAT without parameters]" +
      "\nstatcheck                        displays the stats of the monster [COMBAT]" +
      "\nattack                           the player attacks the enemy, and the enemy attacks the player [COMBAT]" +
      "\nattack    + multiplier [Int]     the player attacks the enemy with a larger force and vice versa [COMBAT]" +
      "\nexamine   + itemname             describes the item that the player can access" +
      "\nuse       + itemname             uses the item (consumable/buff)" +
      "\nuse       + all                  uses all buffs" +
      "\nuse                              uses the most recently acquired usable" +
      "\nequip     + itemname             equips a weapon/armor from the player's inventory" +
      "\nequip     + all                  equips all equippables" +
      "\nequip                            equips the most recently acquired equippable" +
      "\nunequip   + itemname             unequips a weapon/armor from the player's equippables" +
      "\ncheck     + itemname             displays the stats of an item that the player can access (equipped, in inventory, or in shop)" +
      "\nbuy       + itemname             buys the item [SHOP]" +
      "\nbuy       + all                  buys all items [SHOP]" +
      "\nsell      + itemname             sells the item for half the price [SHOP]" +
      "\nhelp                             shows this"
  }


}