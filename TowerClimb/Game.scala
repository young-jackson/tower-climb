package TowerClimb



/** The class `Game` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game. Includes hard-coded stuff specific to
  * this game.*/

class Game {


  /** The title of the adventure game. */
  val title = "Tower Climb"


  /** Creates the shops: */
  private val Shop1 = new Shop(1)
  private val Shop2 = new Shop(2)
  private val Shop3 = new Shop(3)
  private val Shop4 = new Shop(4)


  /** Creates the floors: */
  private val floor0      = new Area(0, None, false)          // Starting floor

  private val floor1main  = new Area(1, None, false)          // Floor 1
  private val floor1shop  = new Area(1, Some(Shop1), false)
  private val floor1arena = new Area(1, None, true)
  private val floor1up    = new Area(1, None, false)

  private val floor2main  = new Area(2, None, false)          // Floor 2
  private val floor2shop  = new Area(2, Some(Shop2), false)
  private val floor2arena = new Area(2, None, true)
  private val floor2up    = new Area(2, None, false)

  private val floor3main  = new Area(3, None, false)          // Floor 3
  private val floor3shop  = new Area(3, Some(Shop3), false)
  private val floor3arena = new Area(3, None, true)
  private val floor3up    = new Area(3, None, false)

  private val floor4main  = new Area(4, None, false)          // Floor 4
  private val floor4shop  = new Area(4, Some(Shop4), false)
  private val floor4arena = new Area(4, None, true)
  private val floor4up    = new Area(4, None, false)

  private val floor5main  = new Area(5, None, false)          // Floor 5
  private val floor5arena = new Area(5, None, true)


          /** Sets the neighbors of each created area: */
          floor0.setNeighbors(Vector("in" -> floor1main))                                         // Starting floor

      floor1main.setNeighbors(Vector(  "arena" -> floor1arena, "shop" -> floor1shop   ))          // Floor 1
      floor1shop.setNeighbors(Vector(  "main"  -> floor1main, "arena" -> floor1arena  ))
     floor1arena.setNeighbors(Vector(  "shop"  -> floor1shop,   "ladder" -> floor1up, "main" -> floor1main   ))
        floor1up.setNeighbors(Vector(  "up"    -> floor2main,  "back" -> floor1arena   ))

      floor2main.setNeighbors(Vector(  "arena" -> floor2arena, "shop" -> floor2shop   ))          // Floor 2
      floor2shop.setNeighbors(Vector(  "main"  -> floor2main, "arena" -> floor2arena  ))
     floor2arena.setNeighbors(Vector(  "shop"  -> floor2shop,   "ladder" -> floor2up, "main" -> floor2main   ))
        floor2up.setNeighbors(Vector(  "up"    -> floor3main, "back" -> floor2arena    ))

      floor3main.setNeighbors(Vector(  "arena" -> floor3arena, "shop" -> floor3shop   ))          // Floor 3
      floor3shop.setNeighbors(Vector(  "main"  -> floor3main, "arena" -> floor3arena  ))
     floor3arena.setNeighbors(Vector(  "shop"  -> floor3shop,   "ladder" -> floor3up, "main" -> floor3main   ))
        floor3up.setNeighbors(Vector(  "up"    -> floor4main, "back" -> floor3arena    ))

      floor4main.setNeighbors(Vector(  "arena" -> floor4arena, "shop" -> floor4shop   ))          // Floor 4
      floor4shop.setNeighbors(Vector(  "main"  -> floor4main, "arena" -> floor4arena  ))
     floor4arena.setNeighbors(Vector(  "shop"  -> floor4shop,   "ladder" -> floor4up, "main" -> floor4main   ))
        floor4up.setNeighbors(Vector(  "up"    -> floor5main, "back" -> floor4arena   ))

      floor5main.setNeighbors(Vector(  "forward" -> floor5arena                         ))        // Floor 5


  /** The character that the player controls in the game. */
  val player = new Player(floor0)


  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = this.player.location.combat.map(_.isOver) == Some(true) && this.player.location.combat.flatMap(_.monster.map(_.boss)) == Some(true) && !this.player.hasDied


  /** Determines whether the player has won, lost, died, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.player.hasDied


  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "\n\nYou see in front of you a tall tower filled with monsters. " +
    "Perhaps you've once even been at this exact location, staring up into the never-ending heights of the massive (5 floor) tower. " +
    "But one thing is certain: \nThe objective of this game is to get to the top of that tower and slay the final boss."


  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete) {
      if (this.player.location.combat.flatMap(_.monster.map(_.name.head)) == Some('J'))
        "\nYou defeated John! Who knew that the one controlling this evil tower was a human/rat! " +
        "\nNevertheless, you have beaten the game, so congratulations!"
      else
        "\n██████████▀▀▀▀▀▀▀▀▀▀▀▀▀██████████" +
        "\n█████▀▀░░░░░░░░░░░░░░░░░░░▀▀█████" +
        "\n███▀░░░░░░░░░░░░░░░░░░░░░░░░░▀███" +
        "\n██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██" +
        "\n█░░░░░░▄▄▄▄▄▄░░░░░░░░▄▄▄▄▄▄░░░░░█" +
        "\n█░░░▄██▀░░░▀██░░░░░░██▀░░░▀██▄░░█" +
        "\n█░░░██▄░░▀░░▄█░░░░░░█▄░░▀░░▄██░░█" +
        "\n██░░░▀▀█▄▄▄██░░░██░░░██▄▄▄█▀▀░░██" +
        "\n███░░░░░░▄▄▀░░░████░░░▀▄▄░░░░░███" +
        "\n██░░░░░█▄░░░░░░▀▀▀▀░░░░░░░█▄░░░██" +
        "\n██░░░▀▀█░█▀▄▄▄▄▄▄▄▄▄▄▄▄▄▀██▀▀░░██" +
        "\n███░░░░░▀█▄░░█░░█░░░█░░█▄▀░░░░███" +
        "\n████▄░░░░░░▀▀█▄▄█▄▄▄█▄▀▀░░░░▄████" +
        "\n███████▄▄▄▄░░░░░░░░░░░░▄▄▄███████" +
        "\n\n Ummmm... Well, i guess you beat the game..."
    } else if (this.player.hasDied) {
      "You died! Time to try again :)"
    } else {
      "Busy or just testing if it works?"
    }

  }


  /** Plays a turn by executing the given in-game command. Returns a textual
    * report of what happened, or an error message if the command was unknown. */
  def playTurn(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.execute(this.player)
    outcomeReport.getOrElse("Unknown command: \"" + command + "\".")
  }


}

