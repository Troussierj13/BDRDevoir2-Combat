package combatDevoir2

import org.apache.spark.rdd.RDD

import scala.collection.immutable.HashMap
import scala.util.Random

case object MainClass extends App {

  var rand = Random

  case class Adjacent(_1 : Entity, _2 : Entity, dist : Integer)

  override def main(args: Array[String]): Unit = {
    super.main(args)

    //Ally
    val angel = new SolarAngel()

    //Ennemies
    val warlord = new Warlord()

    val worgsRider1 = new WorgsRider()
    val worgsRider2 = new WorgsRider()
    val worgsRider3 = new WorgsRider()
    val worgsRider4 = new WorgsRider()
    val worgsRider5 = new WorgsRider()
    val worgsRider6 = new WorgsRider()
    val worgsRider7 = new WorgsRider()
    val worgsRider8 = new WorgsRider()
    val worgsRider9 = new WorgsRider()

    val barbarian1 = new Barbarian()
    val barbarian2 = new Barbarian()
    val barbarian3 = new Barbarian()
    val barbarian4 = new Barbarian()

    //Distances
    val distances: HashMap[(Ally, Enemy), Integer] = HashMap(
      ((angel, barbarian1),120),
      ((angel, barbarian2),125),
      ((angel, warlord),180))

    //Graph
    val graph: Array[(Entity, Array[Entity])] = Array(
      (angel, Array(barbarian1, barbarian2)),
      (barbarian1, Array(angel)),
      (barbarian2, Array(angel))
    )

    graph.foreach(elem => {
      var tuple: (Ally, Enemy) = Tuple2(null, null)
      if (elem._1.isInstanceOf[Ally] == true)
        tuple = Tuple2(elem._1.asInstanceOf[Ally], elem._2(0).asInstanceOf[Enemy])
      else
        tuple = Tuple2(elem._2(0).asInstanceOf[Ally], elem._1.asInstanceOf[Enemy])
      println(distances(tuple))
    })

  }
}