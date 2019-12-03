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
    val angel = new SolarAngel(0, 0)

    //Ennemies

    val barbarian1 = Barbarian(200, 20)
    val barbarian2 = Barbarian(200, 10)
    val barbarian3 = Barbarian(200, 0)
    val barbarian4 = Barbarian(200, -10)
    val barbarian5 = Barbarian(200, -20)
    val barbarian6 = Barbarian(200, -30)

    //Graph
    val graph: Array[(Entity, Array[Entity])] = Array(
      (angel, Array(barbarian1, barbarian2, barbarian3, barbarian4, barbarian5, barbarian6)),
      (barbarian1, Array(angel)),
      (barbarian2, Array(angel)),
      (barbarian3, Array(angel)),
      (barbarian4, Array(angel)),
      (barbarian5, Array(angel)),
      (barbarian6, Array(angel))
    )

    //RDD
    val rdd = sc.makeRDD(graph)

    val messageDegatsCrea: RDD[(Array[Entity], degatMessage)] = rdd.flatMap(elem => {
      val roll = rand.nextInt(20)+1
      var msgs = new ArrayBuffer[(Array[Entity], degatMessage)]()
      var target = new ArrayBuffer[Entity]()

      var i:Int = 0
      while(i < elem._1.getPrecision().length && i < elem._2.length) {
        target += elem._2(i)
        i += 1
      }

      msgs += Tuple2(target.toArray, degatMessage(roll, (elem._1.rangeMelee, elem._1.rangeDist), elem._1.Attack(roll), elem._1.getPrecision()))
    }).cache()

    //PRINT
    PrintRDDMessageCrea(messageDegatsCrea)

  }

  def PrintRDDMessageCrea(rdd:RDD[(Array[Entity], degatMessage)]): Unit = {
    val prt:RDD[String] = rdd.map(elem => {
      var msg:String = ""

      msg += "creature : "
      elem._1.foreach(crea => msg +=  crea.name + " ")
      msg += "\n"
      msg += "degatMessage : \n"
      msg += "\troll :" + elem._2.roll + " \n"
      msg += "\trange :" + elem._2.range + " \n"
      msg += "\tdegats : "
      elem._2.degats.foreach(e =>msg += e + " ")
      msg += "\n"
      msg += "\tprecision : "
      elem._2.precision.foreach(e =>msg += e + " ")
      msg += "\n"

      msg
    })

    prt.foreach(println)
  }
}