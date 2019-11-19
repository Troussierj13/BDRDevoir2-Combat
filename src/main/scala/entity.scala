import scala.util.Random

trait Entity {
  var hp : Integer
  def hpMax : Integer
  def armor : Integer
  def rangeDist : Integer
  def rangeMelee : Integer
  def attackRollDist : Array[Int]
  def attackRollMelee : Array[Int]
  def treshold : Integer
  def rand : Random

  def Attack(roll : Integer, isMelee : Boolean) : Integer
}

trait Enemy extends Entity {
  def speed : Integer
}

trait Ally extends Entity {
}

class WorgsRider extends Enemy{
  override def hpMax : Integer = 13 + 2*(rand.nextInt(10) + 1) +2
  override def speed: Integer = 20
  override var hp: Integer = hpMax
  override def armor: Integer = 18
  override def rangeDist: Integer = 60
  override def rangeMelee: Integer = 5
  override def attackRollDist: Array[Int] = Array(4)
  override def attackRollMelee: Array[Int] = Array(6)
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

class Warlord extends Enemy {
  override def hpMax : Integer = 141 + 13 * (rand.nextInt(10)+1) + 65
  override def speed: Integer = 30
  override var hp: Integer = hpMax
  override def armor: Integer = 27
  override def rangeDist: Integer = 19
  override def rangeMelee: Integer = 5
  override def attackRollDist: Array[Int] = Array(19)
  override def attackRollMelee: Array[Int] = Array(20, 15, 10)
  override def treshold: Integer = 19
  override def rand: Random = new Random()

  override def Attack(roll: Integer, isMelee : Boolean): Integer = {
    var degat = 0;

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

class Barbarian extends Enemy {
  override def hpMax : Integer = 142 + 11 * (rand.nextInt(12) + 1) +65
  override def speed: Integer = 40
  override var hp: Integer = hpMax
  override def armor: Integer = 17
  override def rangeDist: Integer = 110
  override def rangeMelee: Integer = 5
  override def attackRollDist: Array[Int] = Array(16, 11, 6)
  override def attackRollMelee: Array[Int] = Array(19, 14, 9)
  override def treshold: Integer = 19
  override def rand: Random = new Random()

  override def Attack(roll: Integer, isMelee : Boolean): Integer = {
    var degat = 0;

    if(isMelee) {
      degat = rand.nextInt(8) + 11
      if(roll >= treshold) {
        degat = degat*3
      }
    }
    else {
      degat = rand.nextInt(8) + 7
      if(roll >= treshold) {
        degat = degat*3
      }
    }

    degat
  }
}

class SolarAngel extends Ally {
  def regeneration: Integer = 15

  override def hpMax : Integer = 363 + 22 *(rand.nextInt(10)+1)+242
  override var hp: Integer = hpMax
  override def armor: Integer = 44
  override def rangeDist: Integer = 110
  override def rangeMelee: Integer = 10
  override def attackRollDist: Array[Int] = Array(35, 30, 25, 20)
  override def attackRollMelee: Array[Int] = Array(31, 26, 21, 16)
  override def treshold: Integer = 20
  override def rand: Random = new Random()

  override def Attack(roll: Integer, isMelee : Boolean): Integer = {
    var degat = 0;

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