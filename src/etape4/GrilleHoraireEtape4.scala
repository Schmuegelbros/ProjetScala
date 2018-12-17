package etape4

import JaCoP.scala._
import scala.reflect.ClassManifestFactory.classType
/**
 * contraintes pour l etape 4 :
 *  - M. Grolaux donne 20 heures de cours (=10 tranches horaires de 2h)
 *  - M. Grolaux ne souhaite pas donner cours avant 10h30 et après 13h00
 *  - Par serie, chaque cours n'est donne que 4 fois
 *  - M. Grolaux ne donne pas cours le lundi a la serie 1
 *  - Chaque serie a cours dans un local et avec un prof different
 */
object GrilleHoraireEtape4 extends App with jacop {

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

  // - M. Grolaux donne 20 heures de cours (10 tranches horaires)
  val contraintesObligatoiresGrolaux = placerContraintesNombreHeuresADonnerPourProf(1, 10)

  // -M. Grolaux ne souhaite pas donner cours avant 10h30 et après 13h00
  val contraintesSoftsGrolaux = absenceProfAvantHeure(1, 0) ::: //contraintes grolaux enonce
    absenceProfApresHeure(1, 2)
  val contraintesSoft = count(contraintesSoftsGrolaux, 0)

  // Par serie, chaque cours n'est donne que 4 fois
  for (iC <- 1 to cours.size) {
    placerContraintesNombreHeuresCours(iC, 4)
  }
  
  /* TRANCHES HORAIRES */
  for (i <- List.range(0, nTranchesHorairesSem)) {
    // - M.Grolaux ne donne pas cours le lundi a la serie 1
    if (i % 5 == 0) {
      serie1(i)(iProf) #\= 1
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

  val mySearch = search(all_series, most_constrained, indomain_middle)
  val result = minimize(mySearch, contraintesSoft, afficherHoraire)

  /**
   * affichage horaire
   */
  def afficherHoraire(): Unit = {
    var compteur = 1
    val moduloDebut = 3 * nTranchesHorairesJour * nJours

    for (v <- all_series) {
      if (compteur % moduloDebut == 1) {
        println("SERIE " + (compteur / 60 + 1))
        println("%-15s".format("") + "%-30s".format("LUNDI") + "%-32s".format("| MARDI") + "%-32s".format("| MERCREDI") + "%-32s".format("| JEUDI") + "%-30s".format("| VENDREDI"))
        println("---------------------------------------------------------------------------------------------------------------------------------------------------------------------");
      }
      (compteur % moduloDebut) match {
        case 1       => print("%-15s".format("8h30-10h30"))
        case 16      => print("%-15s".format("10h45-12h45"))
        case 31      => print("%-15s".format("13h45-15h45"))
        case 46      => print("%-15s".format("16h00-18h00"))
        case default => print("")
      }
      //print(v.value()+" ")
      v.id() match {
        case "prof" => print(profs.get(v.value()) match {
          case Some(name) => "%-10s".format(name)
          case None       => "%-10s".format("Fourche")
        })
        case "cours" => print(cours.get(v.value()) match {
          case Some(name) => "%-10s".format(name)
          case None       => "%-10s".format("")
        })
        case "local" => print(locaux.get(v.value()) match {
          case Some(name) => "%-10s".format(name)
          case None       => "%-10s".format("")
        })
      }

      if (compteur % 3 == 0) print("| ")
      if (compteur % (3 * nJours) == 0) println()
      if (compteur % moduloDebut == 0) println("\n")

      compteur += 1

    }
  }

  /**
   * Renvoie une liste de BoolVar correspondant aux absences pour un prof donne a une tranche horaire donnee
   * params: indiceProf, l'indice du professeur
   * 				 indiceJour, l'indice du jour correspondant a la tranche horaire
   * 				 indice heure, l'indice de l'heure correspondant a la tranche horaire
   */
  def absenceProfJourHeure(indiceProf: Int, indiceJour: Int, indiceHeure: Int): List[BoolVar] = {
    val indiceTrancheHoraire = indiceHeure * 5 + indiceJour
    val bool = new BoolVar()
    val bool2 = new BoolVar()
    bool <=> (serie1(indiceTrancheHoraire)(iProf) #\= indiceProf)
    bool2 <=> (serie2(indiceTrancheHoraire)(iProf) #\= indiceProf)
    List(bool, bool2)
  }

  /**
   * Renvoie une liste de BoolVar correspondant aux absences pour un prof avant une heure donnee
   * params: indiceProf, l'indice du professeur
   * 				 indiceHeure, l'indice de l'heure qui indique jusque quand le professeur est absent (indice inclus)
   */
  def absenceProfAvantHeure(indiceProf: Int, indiceHeure: Int): List[BoolVar] = {
    val liste1 = for (indice <- List.range(0, indiceHeure * nJours + nTranchesHorairesJour + 1)) yield {
      val bool = new BoolVar()
      bool <=> (serie1(indice)(iProf) #\= indiceProf)
      bool

    };
    val liste2 = for (indice <- List.range(0, indiceHeure * nJours + nTranchesHorairesJour + 1)) yield {
      val bool = new BoolVar()
      bool <=> (serie2(indice)(iProf) #\= indiceProf)
      bool

    };
    liste1 ::: liste2
  }

  /**
   * Renvoie une liste de BoolVar correspondant aux absences pour un prof après une heure donnee
   * params: indiceProf, l'indice du professeur
   * 				 indiceHeure, l'indice de l'heure qui indique a partir de quand le professeur est absent (indice inclus)
   */
  def absenceProfApresHeure(indiceProf: Int, indiceHeure: Int): List[BoolVar] = {
    val liste1 = for (indice <- List.range(indiceHeure * nJours, nTranchesHorairesSem)) yield {
      val bool = new BoolVar()
      bool <=> (serie1(indice)(iProf) #\= indiceProf)
      bool
    };
    val liste2 = for (indice <- List.range(indiceHeure * nJours, nTranchesHorairesSem)) yield {
      val bool = new BoolVar()
      bool <=> (serie2(indice)(iProf) #\= indiceProf)
      bool
    };
    liste1 ::: liste2
  }

  /**
   * Renvoie une liste de BoolVar correspondant aux absences pour un prof a un jour donne
   * params: indiceProf, l'indice du professeur
   * 				 indiceJour, l'indice du jour ou le professeur est absent (0 = lundi, ...)
   */
  def absenceProfJour(indiceProf: Int, indiceJour: Int): List[BoolVar] = {
    val liste = for (indice <- List.range(0, nTranchesHorairesJour)) yield {
      val bool = new BoolVar()
      bool <=> (serie1(indiceJour * nJours + indice)(iProf) #\= indiceProf)
      bool
    };
    val liste2 = for (indice <- List.range(0, nTranchesHorairesJour)) yield {
      val bool = new BoolVar()
      bool <=> (serie2(indiceJour * nJours + indice)(iProf) #\= indiceProf)
      bool
    };
    liste ::: liste2
  }

  /**
   * place les contraintes d'heures a donner (tranches horaires) pour un professeur donne
   * params: indiceProf, l'indice du professeur
   * 				 nbHeures, le nombre d'heures que le professeur doit donner
   */
  def placerContraintesNombreHeuresADonnerPourProf(indiceProf: Int, nbHeures: Int) {
    val liste = for (indice <- List.range(0, nTranchesHorairesSem)) yield {
      val bool = new BoolVar()
      bool <=> (serie1(indice)(iProf) #= indiceProf)
      bool
    };
    val liste2 = for (indice <- List.range(0, nTranchesHorairesSem)) yield {
      val bool2 = new BoolVar()
      bool2 <=> (serie2(indice)(iProf) #= indiceProf)
      bool2
    };
    sum(liste ::: liste2) #= nbHeures
  }

  /**
   * place les contraintes du nombre d'heures qu'un cours doit etre donne
   * params: indiceCours, l'indice du cours
   * 				 nbHeures, le nombre d'heures que le cours doit etre donne
   */
  def placerContraintesNombreHeuresCours(indiceCours: Int, nbHeures: Int) {
    val liste = for (indice <- List.range(0, nTranchesHorairesSem)) yield {
      val bool = new BoolVar()
      bool <=> (serie1(indice)(iCours) #= indiceCours)
      bool
    };
    sum(liste) #= nbHeures
    val liste2 = for (indice <- List.range(0, nTranchesHorairesSem)) yield {
      val bool2 = new BoolVar()
      bool2 <=> (serie2(indice)(iCours) #= indiceCours)
      bool2
    };
    sum(liste2) #= nbHeures
  }

}
