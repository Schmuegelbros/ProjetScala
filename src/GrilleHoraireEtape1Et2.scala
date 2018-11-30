

import JaCoP.scala._
import scala.reflect.ClassManifestFactory.classType
/**
 * contraintes pour l etape 1 : 
 * 	- Chaque cours n est donné que 2 fois par série et par semaine
 *  - M. Grolaux, M. Choquet et M. Damas ne donne pas cours avant 10h45
 *  - Aucun prof ne donne cours le lundi
 *  - Chaque serie a cours dans un local et avec un prof different 
 */
object GrilleHoraireEtape1Et2 extends App with jacop {

  /*	--------------
   *  INITIALISATION
   *  --------------
   */
  
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
    
  /*val coursParProf = Map(
    1 -> List(2,4),
    2 -> List(4),
    3 -> List(1),
    4 -> List(3)
  )*/

  // nombre profs/cours/locaux/series/jours/horaires
  val nProf = 4
  val nCours = 4
  val nLocaux = 2
  val nSeries = 2
  val nJours = 5
  val nTranchesHorairesJour = 4
  val nTranchesHorairesCours = 2
  val nTranchesHorairesSem = nJours * nTranchesHorairesJour

  // tranches horaires serie 1
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
  // - Chaque cours n est donné que 2 fois par série et par semaine
  for (i <- 1 to nCours) {
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
  /*coursParProf.foreach{
    case (key,value) => print (key + " "+value) //TODO 
  }*/
  
  /* TRANCHES HORAIRES */
  for (i <- List.range(0, nTranchesHorairesSem)) {

    // - M. Grolaux, M. Choquet et M. Damas ne donne pas cours avant 10h45
    if (i < 5) {
      serie1(i)(iProf) #\= 3 //M. Choquet
      serie2(i)(iProf) #\= 3
      serie1(i)(iProf) #\= 2 //M. Damas
      serie2(i)(iProf) #\= 2
      serie1(i)(iProf) #\= 1 //M. Grolaux
      serie2(i)(iProf) #\= 1
    }
    // - Aucun prof ne donne cours le lundi
    if (i % 5==0) {
      for(k<- 1 to nCours){
        serie1(i)(iProf) #\= k
        serie2(i)(iProf) #\= k
      }
    }
    
    //- Chaque serie a cours dans un local et avec un prof different (les fourches correspondent au numero 0)
    OR(serie1(i)(iProf) + serie2(i)(iProf) #= 0, serie1(i)(iProf) #\= serie2(i)(iProf)) // prof differents
    OR(serie1(i)(iLocal) + serie2(i)(iLocal) #= 0, serie1(i)(iLocal) #\= serie2(i)(iLocal)) // locaux differents
    
    /* Gestion fourches */
    /* serie 1 */
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

  /**
   * affichage horaire
   */
  def afficherHoraire(): Unit = {
    var compteur = 1
    for (v <- all_series) {
      if (compteur % 60 == 1) {
        println("SERIE "+ (compteur/60+1))
        println("%-15s".format("")+"%-31s".format("| LUNDI")+"%-30s".format("| MARDI")+"%-30s".format("| MERCREDI")+"%-30s".format("| JEUDI")+"%-30s".format("| VENDREDI"))
      }
      (compteur%60) match{
        case 1 => print("%-15s".format("8h30-10h30"))
        case 16 => print("%-15s".format("10h45-12h45"))
        case 31 => print("%-15s".format("13h45-15h45"))
        case 46 => print("%-15s".format("16h00-18h00"))
        case default => print("")
      }
      //print(v.value()+" ")
      v.id() match {
        case "prof" => print(profs.get(v.value()) match {
          case Some(name) => "%-10s".format(name)
          case None => "%-10s".format("Fourche")
        })
        case "cours" => print(cours.get(v.value()) match {
        case Some(name) => "%-10s".format(name)
          case None => "%-10s".format("")
        })
        case "local" => print(locaux.get(v.value()) match {
        case Some(name) => "%-10s".format(name)
          case None => "%-10s".format("")
        })
      }

      if (compteur % 3 == 0) print("| ")
      if (compteur % 15 == 0) println()
      if (compteur % 60 == 0) println("\n")
      
      compteur += 1

    }
  }
}
