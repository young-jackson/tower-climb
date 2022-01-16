package TowerClimb
import scala.io.StdIn._



/**
 * The singleton object `AdventureTextUI` represents a fully text-based version of the
 * Adventure game application. The object serves as a possible entry point for the game,
 * and can be run to start up a user interface that operates in the text console.
 */

object TxtUI extends App {

  /** Creates the game and the player. */
  private val game = new Game
  private val player = game.player
  this.run()


  /** Runs the game. First, a welcome message is printed, then the player gets the chance to
  * play any number of turns until the game is over, and finally a goodbye message is printed. */
  private def run() = {
    println(this.game.welcomeMessage)
    while(!this.game.isOver) {
      this.printAreaInfo()
      this.playTurn()
    }
    println("\n" + this.game.goodbyeMessage)
  }


  /** Prints out a description of the player character's current location, as seen by the character. */
  private def printAreaInfo()= {
    val area = this.player.location
    println("\n\n" + area.name)
    println("-" * area.name.length)
    if (player.isAtShop) println(s"You currently have ${player.wallet}G.\n")
    if(!player.isInCombat){
      println(area.fullDescription + "\n")
    } else {
      println(area.combat.get.info)
    }
  }


  /** Requests a command from the player, plays a game turn accordingly, and prints out a report of what happened.  */
  private def playTurn() = {
    println()
    val command = readLine("Command: ")
    val turnReport = this.game.playTurn(command)
    if(turnReport.nonEmpty) {
      println(turnReport)
    }
  }


}

