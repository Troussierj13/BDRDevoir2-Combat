import scala.util.Random

case object MainClass extends App {

  case class Adjacent(indexAdj : Integer, dist : Integer)

  override def main(args: Array[String]): Unit = {
    super.main(args)

    val angel = new SolarAngel()
    val warlord = new Warlord()
    val worgsRider1 = new WorgsRider()
    val worgsRider2 = new WorgsRider()
    val worgsRider3 = new WorgsRider()

    val graph = Array(
      (angel, Array(1, 2, 3, 4)),
      (warlord, Array(0)),
      (worgsRider1, Array(0)),
      (worgsRider2, Array(0)),
      (worgsRider3, Array(0)))

    var rand = Random
  }
}