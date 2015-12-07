package project

import JaCoP.scala._
import JaCoP.constraints.PrimitiveConstraint

object Main extends jacop {
  def main(args: Array[String]): Unit = {

    type Course = (IntVar, Int, Int)

    def coursMututellementExclusifs(cours1: IntVar, cours2: IntVar): Unit = {
      cours1 + cours2 < 2
    }

    def coursCoRequis(cours1: IntVar, cours2: IntVar): Unit = {
      cours1.>=(cours2)
    }

    def coursPrerequis(cours1: Course, cours2: Course): Unit = {
      coursCoRequis(cours1._1, cours2._1)
      val tmp=IntVar("corequis;"+cours1._1.id()+";"+cours2._1.id(),0,1)
      tmp.==(cours1._1.*(cours2._1))
      tmp
    }

    def coursDejaValide(cours1: Course): Unit = {
      cours1._1 == (0)
    }

    val poo = (IntVar("poo", 0, 1), 1, 1) //todo: 60ects bloc1, 120 ects bloc 2
    val math = (IntVar("math", 0, 1), 1, 1)
    val xml = (IntVar("xml", 0, 1), 1, 1)
    val web = (IntVar("web", 0, 1), 1, 1)
    val paoo = (IntVar("paoo", 0, 1), 4, 1)
    val stage = (IntVar("stage", 0, 1), 4, 2)
    val sec = (IntVar("sec", 0, 1), 1, 2)
    val ihm = (IntVar("ihm", 0, 1), 1, 2)
    val paterns = (IntVar("paterns", 0, 1), 2, 2)
    val nutrition = (IntVar("nutrition", 0, 1), 1, 2)

    val listeCours = List(poo, math, xml, web, paoo, stage, sec, ihm, paterns, nutrition);
    val listeValeurOnPrend = listeCours map ((e) => e._1)

    //mettre cours deja valides dans une liste
    val listeCoursValide = List(poo, math, web, nutrition, xml, ihm, nutrition, sec)
    listeCoursValide.foreach { cours =>
      coursDejaValide(cours)
    }
    val ectsValides=listeCoursValide map (e => e._2) sum
        
    //pre requis:
    //paoo est prerequis de stage TODO/verif si paoo est prerequis de stage et pas l inverse
    val prerequis=Map((stage,paoo))
    
    val listePreToCo= for(key <- prerequis.keys) yield (coursPrerequis(key, prerequis(key)))
    
    //co-requis
    //xmlS est co-requis de web
    val corequis=Map((xml._1,stage._1))
    corequis.keys.foreach{key =>
      coursCoRequis(key, corequis(key))
    }
    
    //cours bloquants
    //sec et ihm sont des cours bloquants:
    coursMututellementExclusifs(sec._1, ihm._1);

    if (ectsValides < 4) { //on est dans le bloc 1
      for (cours <- listeCours if cours._3 == 2) { //pour tous les cours du bloc deux, comme on est dqns le bloc 1
        cours._1.==(0) //on ne les prend pas.
      }
    } else if (ectsValides > 12) {
      //ici on doit prendre tous les cours restqnts non pris
    } else {
      //contrainte 60ects progrqmme de cours etudiqnt
      sum(listeCours map (e => (e._1 * e._2))) >= 6
    }
    
    val result = minimize(search(listeValeurOnPrend, first_fail, indomain_middle),sum(listePreToCo.toList))
    // val result = satisfy(search(listeValeurOnPrend, first_fail, indomain_middle))

    if (result) {

      println("SEMESTRE\nCours\tECTS")
      //TODO: reunir cette ligne ci et lq suivante
      val listeCoursPris = listeCours filter (e => e._1.value() == 1)

      listeCoursPris.foreach { cours =>
        println(cours._1.id() + "\t  " + cours._2)
      }
      val totalEcts = listeCoursPris map (e => e._2) sum

      println("TOTAL\t" + totalEcts);
      println("nombre de prerequis vers corequis\t" + listePreToCo)

    } else println("Aucune solution")

  }

}