package combatDevoir2

import java.util

import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

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
      while(i < elem._1.precision.length && i < elem._2.length) {
        target += elem._2(i)
        i += 1
      }

      getEntityToFight(elem._1,elem._2)

      msgs += Tuple2(target.toArray, degatMessage(roll, (elem._1.rangeMelee, elem._1.rangeDist), elem._1.Attack(roll), elem._1.precision))
    }).cache()

    //PRINT
    PrintRDDMessageCrea(messageDegatsCrea)

  }


  def getEntityToFight(entity: Entity, tabSuivants: Array[Entity]): util.ArrayList[entityToFight] = {
    var listEntityToFigth = new util.ArrayList[entityToFight]
    val x = entity.posX
    val y = entity.posY
    var vector: (Float, Float) = null

    tabSuivants.foreach(e=>{
      vector = getVector(x,y,e.posX,e.posY)
      listEntityToFigth.add(new entityToFight(e,vector,getDistance(vector).toFloat))
    })
    var maxDist=0.0;
    var iSaved = 0
    do{
      maxDist=listEntityToFigth.get(0).distance
      for (i <- 0 to listEntityToFigth.size()-1) {
        if(listEntityToFigth.get(i).distance > maxDist){
          maxDist=listEntityToFigth.get(i).distance
        }
      }

      for (i <- 0 to listEntityToFigth.size()-1) {
        if(listEntityToFigth.get(i).distance == maxDist){
          iSaved = i
        }
      }
      listEntityToFigth.remove(iSaved)
    }while(listEntityToFigth.size()>entity.precision.length)
    listEntityToFigth
  }

  private def getDistance(vector:(Float, Float)) ={
    val distance = Math.sqrt(Math.pow(vector._1,2)+Math.pow(vector._2,2))
    distance
  }
  private def getVector(x1:Float, y1:Float,x2:Float, y2:Float): (Float, Float) ={
    val vector:(Float, Float) = (Math.abs(x1+x2),Math.abs(y1+y2))
    vector
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