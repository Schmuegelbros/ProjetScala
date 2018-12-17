

package etape1et2

import JaCoP.scala._
import scala.reflect.ClassManifestFactory.classType
/**
 * contraintes pour l etape 1 : 
 * 	- Chaque cours n est donne que 2 fois par serie et par semaine
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
    1 -> "Infra(ex)",
    2 -> "PLFC",
    3 -> "Web3",
    4 -> "Pattern")

  val locaux = Map(
    1 -> "017",
    2 -> "019")

  // nombre profs/cours/locaux/series/jours/horaires
  val nProf = profs.size
  val nCours = cours.size
  val nLocaux = locaux.size
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
  // - Chaque cours n est donne que 2 fois par serie et par semaine
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
    sum(coursTempS1) #= 2 // Par serie, chaque cours dure exactement 2 tranches horaires (4h)
    sum(coursTempS2) #= 2 // Par serie, chaque cours dure exactement 2 tranches horaires (4h)
  }

  
  /* TRANCHES HORAIRES */
  
  for (i <- List.range(0, nTranchesHorairesSem)) {
    /* CONTRAINTES PROFESSORALES */
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
        println("%-15s".format("")+"%-30s".format("LUNDI")+"%-32s".format("| MARDI")+"%-32s".format("| MERCREDI")+"%-32s".format("| JEUDI")+"%-30s".format("| VENDREDI"))
        println("---------------------------------------------------------------------------------------------------------------------------------------------------------------------");
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

