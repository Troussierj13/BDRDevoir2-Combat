package combatDevoir2

import org.apache.spark.{SparkConf, SparkContext, rdd}
import org.apache.spark.rdd.RDD

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case object MainClass extends App {

  var rand = Random

  case class degatMessage(roll: Int, range: (Integer, Integer), degats: Array[(Integer, Integer)], precision: Array[(Integer, Integer)])

  override def main(args: Array[String]): Unit = {
    super.main(args)

    val conf = new SparkConf()
      .setAppName("Devoir1 BDR")
      .setMaster("local[*]")
    val sc = new SparkContext(conf)
    sc.setLogLevel("ERROR")

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

    //RDD
    val rdd = sc.makeRDD(graph)

    val messageDegatsCrea: RDD[(Array[Entity], degatMessage)] = rdd.flatMap(elem => {
      var roll = rand.nextInt(20)+1
      val msgs = new ArrayBuffer[(Array[Entity], degatMessage)]()

      msgs += Tuple2(elem._2, degatMessage(roll, (elem._1.rangeMelee, elem._1.rangeDist), elem._1.Attack(roll), elem._1.getPrecision()))
    }).cache()
  }
}