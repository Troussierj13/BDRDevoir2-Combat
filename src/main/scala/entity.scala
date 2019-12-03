package combatDevoir2

import scala.util.Random

trait Entity {
  var hp : Integer
  def posX : Float
  def posY : Float
  def name : String
  def hpMax : Integer
  def armor : Integer
  def rangeDist : Integer
  def rangeMelee : Integer
  def attackRollDist : Array[Int]
  def attackRollMelee : Array[Int]
  def attackRoll : Array[(Integer, Integer)]
  def treshold : Integer
  def rand : Random

  def Attack(roll: Integer, isMelee : Boolean): Integer
  def getPrecision(): Array[(Integer, Integer)] = {
    var size = if(attackRollMelee.size>attackRollDist.size) attackRollMelee.size else attackRollDist.size

    var result:Array[(Integer, Integer)] = new Array[(Integer, Integer)](size)

    for(i <- 0 to size-1) {
      var precisionDist = 0
      var precisionMelee = 0

      if (i < attackRollMelee.size) {
        precisionMelee = attackRollMelee(i)
      }

      if (i < attackRollDist.size) {
        precisionDist = attackRollDist(i)
      }

      result(i) = (precisionMelee, precisionDist)
    }
    result
  }
  def Attack(roll: Integer): Array[(Integer, Integer)] = {
    var size = if(attackRollMelee.size>attackRollDist.size) attackRollMelee.size else attackRollDist.size

    var result:Array[(Integer, Integer)] = new Array[(Integer, Integer)](size)

    for(i <- 0 to size-1) {
      var degatDist = 0
      var degatMelee = 0

      if (i < attackRollMelee.size) {
        degatMelee = Attack(roll, true)
      }

      if (i < attackRollDist.size) {
        degatDist = Attack(roll, false)
      }

      result(i) = (degatMelee, degatDist)
    }
    result
  }
}

abstract class Enemy extends Entity {
  def speed : Integer = 0
}

abstract class Ally extends Entity {
}

case class WorgsRider(x: Float, y: Float) extends Enemy {
  override def name : String = "WorgsRider"
  override def posX: Float = x
  override def posY: Float = y
  override def hpMax : Integer = 13 + 2*(rand.nextInt(10) + 1) +2
  override def speed: Integer = 20
  override var hp: Integer = hpMax
  override def armor: Integer = 18
  override def rangeDist: Integer = 60
  override def rangeMelee: Integer = 5
  override def attackRollDist: Array[Int] = Array(4)
  override def attackRollMelee: Array[Int] = Array(6)
  override def attackRoll : Array[(Integer, Integer)] = Array((6, 4))
  override def treshold: Integer = 20
  override def rand: Random = new Random()

  override def Attack(roll: Integer, isMelee : Boolean): Integer = {
    var degat = 0;

    if(isMelee) {
      degat = rand.nextInt(8) + 3
      if(roll >= treshold) {
        degat = degat*3
      }
    }
    else {
      degat = rand.nextInt(6) + 1
      if(roll >= treshold) {
        degat = degat*3
      }
    }

    degat
  }
}

case class Warlord(x: Float, y: Float) extends Enemy {
  override def name : String = "Warlord"
  override def posX: Float = x
  override def posY: Float = y
  override def hpMax : Integer = 141 + 13 * (rand.nextInt(10)+1) + 65
  override def speed: Integer = 30
  override var hp: Integer = hpMax
  override def armor: Integer = 27
  override def rangeDist: Integer = 19
  override def rangeMelee: Integer = 5
  override def attackRollDist: Array[Int] = Array(19)
  override def attackRollMelee: Array[Int] = Array(20, 15, 10)
  override def attackRoll : Array[(Integer, Integer)] = Array((20, 19), (15, 0), (10, 0))
  override def treshold: Integer = 19
  override def rand: Random = new Random()

  override def Attack(roll: Integer, isMelee : Boolean): Integer = {
    var degat = 0

    if(isMelee) {
      degat = rand.nextInt(8) + 11
      if(roll >= treshold) {
        degat = degat+2*(rand.nextInt(6)+1)
      }
    }
    else {
      degat = rand.nextInt(6) + 6
      if(roll >= treshold) {
        degat = degat+2*(rand.nextInt(6)+1)
      }
    }

    degat
  }
}

case class Barbarian(x: Float, y: Float) extends Enemy {
  override def name : String = "Barbarian"
  override def posX: Float = x
  override def posY: Float = y
  override def hpMax: Integer = 142 + 11 * (rand.nextInt(12) + 1) + 65
  override def speed: Integer = 40
  override var hp: Integer = hpMax
  override def armor: Integer = 17
  override def rangeDist: Integer = 110
  override def rangeMelee: Integer = 5
  override def attackRollDist: Array[Int] = Array(16, 11, 6)
  override def attackRollMelee: Array[Int] = Array(19, 14, 9)
  override def attackRoll : Array[(Integer, Integer)] = Array((19, 16), (14, 11), (9, 6))
  override def treshold: Integer = 19
  override def rand: Random = new Random()

  def Attack(roll: Integer, isMelee: Boolean): Integer = {
    var degat = 0

    if (isMelee) {
      degat = rand.nextInt(8) + 11
      if (roll >= treshold) {
        degat = degat * 3
      }
    }
    else {
      degat = rand.nextInt(8) + 7
      if (roll >= treshold) {
        degat = degat * 3
      }
    }

    degat
  }
}

case class SolarAngel(x: Float, y: Float) extends Ally {
  def regeneration: Integer = 15

  override def name : String = "SolarAngel"
  override def posX: Float = x
  override def posY: Float = y
  override def hpMax : Integer = 363 + 22 *(rand.nextInt(10)+1)+242
  override var hp: Integer = hpMax
  override def armor: Integer = 44
  override def rangeDist: Integer = 110
  override def rangeMelee: Integer = 10
  override def attackRollDist: Array[Int] = Array(35, 30, 25, 20)
  override def attackRollMelee: Array[Int] = Array(31, 26, 21, 16)
  override def attackRoll : Array[(Integer, Integer)] = Array((31, 35), (26, 30), (21, 25), (16, 20))
  override def treshold: Integer = 20
  override def rand: Random = new Random()

  override def Attack(roll: Integer, isMelee : Boolean): Integer = {
    var degat = 0

    if(isMelee) {
      degat = 2*(rand.nextInt(6) + 1) + 18
    }
    else {
      degat = 2*(rand.nextInt(6) + 1) + 14
    }

    degat
  }

  def Regenerate(): Unit = {
    hp += regeneration
    if(hp>hpMax) {
      hp = hpMax
    }
  }
}