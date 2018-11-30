package etape1

import JaCoP.scala._
import scala.reflect.ClassManifestFactory.classType

object GrilleHoraire extends App with jacop {

  /*	--------------
   *  INITIALISATION
   *  --------------
   */
  
  //test
  
  //indices
  val iProf = 0
  val iCours = 1
  val iLocal = 2

  val profs = Map(
    1 -> "Grolaux",
    2 -> "Damas",
    3 -> "Choquet",
    4 -> "Leleux")
  val cours = Map(
    1 -> "Infra",
    2 -> "PLFC",
    3 -> "Web3",
    4 -> "Pattern")

  val locaux = Map(
    1 -> "017",
    2 -> "019")
    
  val coursParProf = Map(
    1 -> List(2,4),
    2 -> List(4),
    3 -> List(1),
    4 -> List(3)
    
    
    
  )

  // nombre profs/cours/locaux/series/jours/horaires
  val nProf = 4
  val nCours = 4
  val nLocaux = 2
  val nSeries = 2
  val nJours = 5
  val nTranchesHorairesJour = 4
  val nTranchesHorairesCours = 2
  val nTranchesHorairesSem = nJours * nTranchesHorairesJour

  // tranches horaire serie 1
  val serie1 =
    for (i <- List.range(0, nTranchesHorairesSem)) yield List(
      new IntVar("prof", 0, nProf),
      new IntVar("cours", 0, nCours),
      new IntVar("local", 0, nLocaux))

  // tranches horaires serie 2
  val serie2 =
    for (i <- List.range(0, nTranchesHorairesSem)) yield List(
      new IntVar("prof", 0, nProf),
      new IntVar("cours", 0, nCours),
      new IntVar("local", 0, nLocaux))

  /* -----------
   * CONTRAINTES
   * -----------
   */

  /* COURS */
  for (i <- 1 to nCours) {
    //print("INDICE"+i)
    val coursTempS1 = for (j <- List.range(0, nTranchesHorairesSem)) yield {
      val b = new BoolVar("coursTempS1");
      b <=> (serie1(j)(iCours) #= i)
      b
    }
    val coursTempS2 = for (j <- List.range(0, nTranchesHorairesSem)) yield {
      val b = new BoolVar("coursTempS2");
      b <=> (serie2(j)(iCours) #= i)
      b
    }
    sum(coursTempS1) #= 2 // 2 tranche horaires pour chaque cours par serie
    sum(coursTempS2) #= 2 // 2 tranche horaires pour chaque cours par serie
  }
  
  /* COURS PAR PROF*/
  coursParProf.foreach{
    case (key,value) => print (key + " "+value) //TODO 
  }
  
  /* TRANCHES HORAIRES */
  for (i <- List.range(0, nTranchesHorairesSem)) {

    // test OK : prof num 3 && 2 ne donne pas cours avant 10h30
    if (i < 5) {
      serie1(i)(iProf) #\= 3
      serie2(i)(iProf) #\= 3
      serie1(i)(iProf) #\= 2
      serie2(i)(iProf) #\= 2
      serie1(i)(iProf) #\= 1
      serie2(i)(iProf) #\= 1
    }
    // aucun prof donne cours lundi
    if (i % 5==0) {
      for(k<- 1 to nCours){
        serie1(i)(iProf) #\= k
        serie2(i)(iProf) #\= k
      }
    }
    
    OR(serie1(i)(iProf) + serie2(i)(iProf) #= 0, serie1(i)(iProf) #\= serie2(i)(iProf)) // prof différents
    OR(serie1(i)(iLocal) + serie2(i)(iLocal) #= 0, serie1(i)(iLocal) #\= serie2(i)(iLocal)) // locaux différents
    

    /* serie 1 */
    /* gestion fourches */
    //boolvar pour voir si prof = 0
    val boolVarProfSerie1 = new BoolVar("boolProfS1");
    boolVarProfSerie1 <=> (serie1(i)(iProf) #= 0)

    //boolvar pour voir si cours = 0
    val boolVarCoursSerie1 = new BoolVar("boolCoursS1");
    boolVarCoursSerie1 <=> (serie1(i)(iCours) #= 0)

    //boolvar pour voir si local = 0
    val boolVarLocalSerie1 = new BoolVar("boolLocalS1");
    boolVarLocalSerie1 <=> (serie1(i)(iLocal) #= 0)

    OR(
      sum(List(boolVarCoursSerie1, boolVarProfSerie1, boolVarLocalSerie1)) #= 0,
      sum(List(boolVarCoursSerie1, boolVarProfSerie1, boolVarLocalSerie1)) #= 3)

    /* serie 2 */
    //boolvar pour voir si prof = 0
    val boolVarProf = new BoolVar("boolProf");
    val primitConstraintProf = (serie2(i)(iProf) #= 0)
    boolVarProf <=> primitConstraintProf

    //boolvar pour voir si cours = 0
    val boolVarCours = new BoolVar("boolCours");
    val primitConstraintCours = (serie2(i)(iCours) #= 0)
    boolVarCours <=> primitConstraintCours

    //boolvar pour voir si local = 0
    val boolVarLocal = new BoolVar("boolLocal");
    val primitConstraintLocal = (serie2(i)(iLocal) #= 0)
    boolVarLocal <=> primitConstraintLocal

    OR(
      sum(List(boolVarCours, boolVarProf, boolVarLocal)) #= 0,
      sum(List(boolVarCours, boolVarProf, boolVarLocal)) #= 3)

  }

  /* --------
   * RESEARCH
   * --------
   */
  val all_series = serie1.flatten ::: serie2.flatten

  //val mySearch = search(all_series, first_fail, indomain_middle)
  val mySearch = search(all_series, most_constrained, indomain_middle)
  val result = satisfy(mySearch, afficherHoraire)

  def afficherHoraire(): Unit = {
    var compteur = 1
    for (v <- all_series) {
      //print(v.value()+" ")
      v.id() match {
        case "prof" => print(profs.get(v.value()) match {
          case Some(name) => "%-10s".format(name)
          case None => "%-10s".format("X")
        })
        case "cours" => print(cours.get(v.value()) match {
        case Some(name) => "%-10s".format(name)
          case None => "%-10s".format("X")
        })
        case "local" => print(locaux.get(v.value()) match {
        case Some(name) => "%-10s".format(name)
          case None => "%-10s".format("X")
        })
      }

      if (compteur % 3 == 0) print("| ")
      if (compteur % 15 == 0) println()
      if (compteur % 60 == 0) println("\n")
      compteur += 1

    }
  }
}
