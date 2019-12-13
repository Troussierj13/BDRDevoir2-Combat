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

    val barbarian1 = Barbarian(500, 20)
    val barbarian2 = Barbarian(500, 10)
    val barbarian3 = Barbarian(500, 0)
    val barbarian4 = Barbarian(500, -10)
    val barbarian5 = Barbarian(500, -20)
    val barbarian6 = Barbarian(500, -30)

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
    var rdd = sc.makeRDD(graph)

    //Check de fin
    var foundEnemy = false
    var foundAlly = false

    do {
      //Boolean == true => melee attack
      val messageDegatsCreatures: RDD[(Array[Entity], (degatMessage, Boolean))] = rdd.flatMap(elem => {
        val roll = rand.nextInt(20)+1
        val degatM = degatMessage(roll, (elem._1.rangeMelee, elem._1.rangeDist), elem._1.Attack(roll), elem._1.precision)
        var msgs = new ArrayBuffer[(Array[Entity], (degatMessage, Boolean))]()

        //Recup Tuple2(cibleMelee: Array(Entity), cibleDistance: Array(Entity))
        var target = elem._1.Behaviour(getEntityToFight(elem._1, elem._2).asScala.toArray)

        if(target._1.length > 0)
          msgs += Tuple2(target._1, (degatM, true))
        if(target._2.length > 0)
          msgs += Tuple2(target._2, (degatM, false))

        msgs
      })

      // Creer un PairRDD (cible: Entity, degat: Integer)
      // à partir du rdd qui contient (fightableEntity: Array(Entity), infoDegat: degatMessage, isMelee: Boolean)
      var pairRDDCreatureDegat: RDD[(Entity, Integer)] = messageDegatsCreatures.flatMap(elem => {
        var msgs = new ArrayBuffer[(Entity, Integer)]()

        var iEntity = 0
        var iDegat = 0
        var degat = 0

        //On parcourt les x attaques possible
        elem._2._1.precision.foreach(p => {
          //Si cible melee
          if(elem._2._2) {
            if (p._1 > 0) { // si precision >0 alors on peut attaquer
              if (elem._1(iEntity).armor < p._1 + elem._2._1.roll) {  // On verifie que le lancer de dé + la précision > l'armure de la cible
                degat += elem._2._1.degats(iDegat)._1
              }
              iDegat += 1
              if (elem._1(iEntity).hp - degat <= 0) { // on verifie que les degat à appliqué tue la cible pour passer a la cible suivant et réinitialiser les dégats
                iEntity += 1
                degat = 0
              }

              if (degat > 0) { // si des dégats sont à appliquer alors on envoi un message de la forme (entityCible, 33)
                msgs += Tuple2(elem._1(iEntity), degat)
                degat = 0
              }
            }
          }
          else { //si cible distance
            if (p._2 > 0) {
              if (elem._1(iEntity).armor < p._2 + elem._2._1.roll) {
                degat += elem._2._1.degats(iDegat)._2
              }
              iDegat += 1
              if (elem._1(iEntity).hp - degat <= 0) {
                iEntity += 1
                degat = 0
              }

              if (degat > 0) {
                msgs += Tuple2(elem._1(iEntity), degat)
                degat = 0
              }
            }
          }
        })
        msgs
      })

      //On reduce sur la creature tout les degats qu'elle a recu
      val finalMessageDegat = pairRDDCreatureDegat.reduceByKey(_+_).collect()

      //On applique les degats du pairRdd qui definit les degat que chaque cible a pris sur le graph
      rdd = rdd.map(elem => {
        finalMessageDegat.foreach(s => {
          if (elem._1 == s._1) { // le l'entite courant de la map() == la clé du pairRDD, alors on inflige les degats
            elem._1.hp -= s._2
          }
          println("Entity : " + elem._1 + ",       Pos = " + elem._1.posX + "," + elem._1.posY)
        })

        //TODO : Faire la même des degats infligé mais pour le déplacement

        elem
      }).cache()

      foundEnemy = false
      foundAlly = false

      rdd.collect().foreach(elem => {
        println("Entity : " + elem._1 + ",       HP = " + elem._1.hp)
        if(elem._1.hp>0) {
          elem._1 match {
            case _: Enemy => foundEnemy = true
            case _: Ally => foundAlly = true
            case _ =>
          }
        }
      })

    } while(foundEnemy&foundAlly)

    println("Finish")
  }

  //Return les X cible les plus proche, X etant le nombre d'attaque possible
  def getEntityToFight(entity: Entity, tabSuivants: Array[Entity]): util.ArrayList[entityToFight] = {
    var listEntityToFigth = new util.ArrayList[entityToFight]
    val x = entity.posX
    val y = entity.posY

    tabSuivants.foreach(e=>{
      val vector = getVector((x,y),(e.posX,e.posY))
      val distance = getDistance(vector)
      listEntityToFigth.add(new entityToFight(e,vector,distance))
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