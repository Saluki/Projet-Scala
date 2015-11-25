package project

import JaCoP.scala._

object Main extends jacop {
  var ectsValides = 0
  def main(args: Array[String]): Unit = {

    type Course = (IntVar, Int, Int) //prend/prend pas, ects 

    def coursMututellementExclusifs(cours1: IntVar, cours2: IntVar): Unit = {
      cours1 + cours2 < 2
    }

    def coursCoRequis(cours1: IntVar, cours2: IntVar): Unit = {
      cours1.>=(cours2)
    }

    def coursPrerequis(cours1: Course, cours2: Course, listeCoursValide: List[Course]): Unit = {
      if (!listeCoursValide.contains(cours1)) {
        cours2._1.==(0)
      }
    }
    
     def coursDejaValide(cours1: Course): Unit = {
       cours1._1==(0)
       ectsValides+=cours1._2
     }
    

    /**
     * cours(intvar, ects:int, bloc:int, dejavalide:int)
     *
     * Values de l intvar
     * 0 = Cours jamais pris
     * 1 = Veut prendre cours
     *
     */
    val poo = (IntVar("poo", 0, 1), 2, 1) //todo: 60ects bloc1, 120 ects bloc 2
    val math = (IntVar("math", 0, 1), 1, 1)
    val xml = (IntVar("xml", 0, 1), 3, 1)
    val web = (IntVar("web", 0, 1), 2, 1)
    val paoo = (IntVar("paoo", 0, 1), 2, 1)
    val stage = (IntVar("stage", 0, 1), 1, 2)
    val sec = (IntVar("sec", 0, 1), 2, 2)
    val ihm = (IntVar("ihm", 0, 1), 3, 2)
    val paterns = (IntVar("paterns", 0, 1), 2, 2)
    val nutrition = (IntVar("nutrition", 0, 1), 3, 2)

    val listeCours = List(poo, math, xml, web, paoo, stage, sec, ihm, nutrition, paterns);

    val listeValeurOnPrend = listeCours map ((e) => e._1)

    //mettre cours deja valides dans une liste
    val listeCoursValide = List()
    listeCoursValide.foreach { cours =>
      coursDejaValide(cours)
    }
    //pre requis:
    //paoo est prerequis de stage
    coursPrerequis(paoo, stage, listeCoursValide);

    //co-requis
    //xmlS est co-requis de web
    coursCoRequis(xml._1, web._1)

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
      poo._1 * poo._2 + math._1 * math._2 + xml._1 * xml._2 + web._1 * web._2 + paoo._1 * paoo._2 + stage._1 * stage._2 + sec._1 * sec._2 + ihm._1 * ihm._2 + nutrition._1 * nutrition._2 + paterns._1 * paterns._2 > 6
    }

    val result = satisfy(search(listeValeurOnPrend, first_fail, indomain_middle))

    if (result) {
      
      println("SEMESTRE\nVous avez comme cours: ")
      
      listeCours.foreach { cours =>
        
        if(cours._1.value()==1) {
        	println(cours._1.id())
        }
      }
    } else println("Aucune solution")

  }

}