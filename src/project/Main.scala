package project

import JaCoP.scala._

object Main extends jacop {
  def main(args: Array[String]): Unit = {

    //type course = (IntVar, Int) //prend/prend pas, ects 

    /**
     * cours(intvar, ects:int, bloc:int)
     *
     *
     * Values de l intvar
     * 0 = Cours jamais pris
     * 1 = Veut prendre cours
     * 2 = Deja pris l'annee prec.
     *
     *
     */
    val poo = (IntVar("poo", 0, 2), 6, 1) //todo: 60ects bloc1, 120 ects bloc 2
    val math = (IntVar("m", 0, 2), 4, 1)
    val xml = (IntVar("x", 0, 2), 3, 1)
    val web = (IntVar("w", 0, 2), 2, 1)
    val paoo = (IntVar("p", 0, 2), 2, 1)
    val stage = (IntVar("s", 0, 2), 1, 1)
    val sec = (IntVar("sec", 0, 2), 2, 2)
    val ihm = (IntVar("ihm", 0, 2), 4, 2)
    val aaa = (IntVar("aaa", 0, 2), 4, 2)
    val bbb = (IntVar("bbb", 0, 2), 3, 2)

    val listeCours = List(poo, math, xml, web, paoo, stage, sec, ihm, aaa, bbb);

    var onprend = List(poo._1)
    listeCours.foreach { cours =>
      onprend = onprend.::(cours._1)
    }
    onprend = onprend.distinct

    //mettre cours deja valides a 2
    val listeCoursValide = List(math, poo)
    listeCoursValide.foreach { cours =>
      cours._1.==(2)
    }

    //pre requis:
    //paoo est prerequis de stage
    //paoo._1.
    
    //co-requis
    //paoo est co-requis de web
    xml._1.>=(web._1)

    //cours bloquants
    //sec et ihm sont des cours bloquants:
    sum(List(sec._1, ihm._1)) #= 1    
    
    var ectsValides = 0
    listeCoursValide.foreach { cours =>
      ectsValides += cours._2;
    }
    if (ectsValides < 45) { 						//on est dans le bloc 1
      for (cours <- listeCours if cours._3 == 2) { 	//pour tous les cours du bloc deux, comme on est dqns le bloc 1
        cours._1.==(0) 								//on ne les prend pas.
      }

    } else if (ectsValides > 120) {
      //on peut supprimer ce truc
    } else {
      //au moins 60 ects
    }

    val result = satisfy(search(onprend, first_fail, indomain_middle))

    if (result) {
      listeCours.foreach { cours =>
        println(cours._1)
      }
    } else println("Aucune solution")
  }

}