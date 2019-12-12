package combatDevoir2

import java.util
import java.util.Collections

import combatDevoir2.MainClass.degatMessage
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
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

    val barbarian1 = Barbarian(400, 20)
    val barbarian2 = Barbarian(400, 10)
    val barbarian3 = Barbarian(400, 0)
    val barbarian4 = Barbarian(400, -10)
    val barbarian5 = Barbarian(400, -20)
    val barbarian6 = Barbarian(400, -30)

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

    //Boolean == true => attack melee
    val messageDegatsCrea: RDD[(Array[Entity], (degatMessage, Boolean))] = rdd.flatMap(elem => {
      val roll = rand.nextInt(20)+1
      val degatM = degatMessage(roll, (elem._1.rangeMelee, elem._1.rangeDist), elem._1.Attack(roll), elem._1.precision)
      var msgs = new ArrayBuffer[(Array[Entity], (degatMessage, Boolean))]()

      var target = elem._1.Behaviour(getEntityToFight(elem._1, elem._2).asScala.toArray)

      if(target._1.length > 0)
        msgs += Tuple2(target._1, (degatM, true))
      if(target._2.length > 0)
        msgs += Tuple2(target._2, (degatM, false))

      msgs
    }).cache()
  }

  def getEntityToFight(entity: Entity, tabSuivants: Array[Entity]): util.ArrayList[entityToFight] = {
    var listEntityToFigth = new util.ArrayList[entityToFight]
    val x = entity.posX
    val y = entity.posY

    tabSuivants.foreach(e=>{
      val vector = getVector((x,y),(e.posX,e.posY))
      val distance = getDistance(vector)
      listEntityToFigth.add(new entityToFight(e,vector,getDistance(vector)))
    })

    Collections.sort(listEntityToFigth)

    while(listEntityToFigth.size()>entity.precision.length){
      listEntityToFigth.remove(entity.precision.length)
    }

    listEntityToFigth
  }

  private def getDistance(vector:(Float, Float)): Float ={
    Math.sqrt(Math.pow(vector._1,2)+Math.pow(vector._2,2)).toFloat
  }
  private def getVector(vectorOrigin:(Float, Float), vectorTarget:(Float, Float)): (Float, Float) ={
    (vectorTarget._1-vectorOrigin._1,vectorTarget._2-vectorOrigin._2)
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