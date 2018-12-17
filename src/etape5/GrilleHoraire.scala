package etape5

import JaCoP.scala._
import scala.reflect.ClassManifestFactory.classType
import scala.collection.mutable.ListBuffer

/**
 * contraintes pour l etape 5 :
 *  - Chaque prof ne donne que certains cours
 *  - Chaque cours n est donne qu'un certain nombre de fois
 * 	- Gestion des fourches (intvar à zero representent les fourches --> soit tous a 0 soit aucun a 0)
 *  - Chaque serie a cours dans un local et avec un prof different
 */
object GrilleHoraire extends App with jacop {

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
    4 -> "Leleux",
    5 -> "DeZanger",
    6 -> "Ninane",
    7 -> "Tribel",
    8 -> "VanEdbg.")

  /*val cours1BINTh = Map(
    1 -> "APOO (th)", 2 -> "Algo 1 (th)", 3 -> "DO", 4 -> "Anglais 1", 5 -> "Compta", 6 -> "Eco", 7 -> "Math 1 (th)")

  val cours1BINEx = Map(
    1 -> "APOO (ex)", 2 -> "Algo 1 (ex)", 3 -> "Math 1 (ex)")*/

  val cours = Map(
    1 -> "Infra",
    2 -> "PLFC",
    3 -> "Web3",
    4 -> "Pattern",
    5 -> "Big Data",
    6 -> "Anglais",
    7 -> "Secu",
    8 -> "DotNet",
    9 -> "Agile")

  val locaux = Map(
    1 -> "AudA",
    2 -> "AudB",
    3 -> "B11",
    4 -> "B12",
    5 -> "B21",
    7 -> "Labo")

  // nombre profs/cours/locaux/series/jours/horaires
  val nSeries = 2
  val nJours = 5
  val nTranchesHorairesJour = 4
  val nTranchesHorairesCours = 2
  val nTranchesHorairesSem = nJours * nTranchesHorairesJour

  // initialisation bloc 3 uniquement (recherche sans fin de solution si les series des autres annees sont initialisees)
  val series = initSeries(3, 2) // ++ initSeries(2, 3) ++ initSeries(1, 4)
  val listeIntVar = series.values.toList;
  val listeIntVarFlattened = listeIntVar.flatten

  // - Chaque prof ne donne que certains cours
  initialiserContraintesProfDonne1SeulCours(2, 4)
  initialiserContraintesProfDonne1SeulCours(5, 6)
  initialiserContraintesProfDonne1SeulCours(6, 7)
  initialiserContraintesProfDonne1SeulCours(7, 9)
  initialiserContraintesProfDonne1SeulCours(8, 3)
  initialiserContraintesProfDonne2Cours(1, 2, 4)
  initialiserContraintesProfDonne2Cours(3, 1, 8)
  //initialiserContraintesProfDonne2Cours(4, 3, 5) // commente car recherche sans fin de solution si tous les professeurs ont des contraintes

  // - Chaque cours n est donne qu'un certain nombre de fois
  initialiserHeuresParCours(2, 1);
  initialiserHeuresParCours(1, 6)
  initialiserHeuresParCours(1, 5)
  initialiserHeuresParCours(2, 7)
  initialiserHeuresParCours(1, 8)
  initialiserHeuresParCours(1, 9)
  initialiserHeuresParCours(1, 2)
  initialiserHeuresParCours(2, 3)
  initialiserHeuresParCours(1, 4)

  /* -----------
   * CONTRAINTES
   * -----------
   */
  // - Gestion des fourches
  gestionFourches();
  // - Gestion conflits horaires profs
  gestionConflitsHorairesProfs();
  /* COURS */
  /*val contraintesSoftDamas = absenceProfJour(2, 0) :::absenceProfJour(1, 0):::absenceProfJour(3, 0):::absenceProfJour(4, 0)
  sum(contraintesSoftDamas, contraintesSoftDamas.size)

  val contraintesSoftChoquet = absenceProfApresHeure(3, 2)
  sum(contraintesSoftChoquet, contraintesSoftChoquet.size)

  val contraintesSoftGrolaux = absenceProfAvantHeure(1, 0)
  sum(contraintesSoftGrolaux, contraintesSoftGrolaux.size)*/

  // - M. Grolaux donne 20 heures de cours
  //val contraintesObligatoiresGrolaux = placerContraintesNombreHeuresADonnerPourProf(1, 10)

  // -M. Grolaux ne souhaite pas donné cours avant 10h30 et après 13h00
  /*val contraintesSoftsGrolaux= absenceProfAvantHeure(1, 0)::: //contraintes grolaux enonce
                         absenceProfApresHeure(1, 2)

   val contraintesSoft=count(contraintesSoftsGrolaux,0)



 */

  /* --------
   * RESEARCH
   * --------
   */
  val all_series = series.values.flatten.toList.flatten

  val mySearch = search(all_series, most_constrained, indomain_middle)
  val result = satisfy(mySearch, afficherHoraire)

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
   * S'assure que deux profs ne peuvent donner cours a deux series simultanement
   */
  def gestionConflitsHorairesProfs() {
    for (iSer1 <- List.range(0, series.size)) {
      for (iSer2 <- List.range(0, series.size)) {
        if (iSer1 < iSer2) {
          for (iTH <- List.range(0, nTranchesHorairesSem)) {
            OR(listeIntVar(iSer1)(iTH)(iProf) + listeIntVar(iSer2)(iTH)(iProf) #= 0, listeIntVar(iSer1)(iTH)(iProf) #\= listeIntVar(iSer2)(iTH)(iProf))
          }
        }
      }
    }
  }

  /**
   * place les contraintes du nombre d'heures qu'un cours doit etre donne
   * params: cours, l'indice du cours
   * 				 nbHeures, le nombre d'heures que le cours doit etre donne
   */
  def initialiserHeuresParCours(nbHeures: Int, cours: Int) {
    for (serie <- List.range(0, series.size)) yield {
      val liste = for (indice <- List.range(0, nTranchesHorairesSem)) yield {
        val bool = new BoolVar()
        bool <=> (listeIntVar(serie)(indice)(iCours) #= cours)
        bool

      };

      sum(liste) #= nbHeures
    }

  }

  /**
   * Contraint a ce qu'un prof donne uniquement le cours passe en parametre
   * params:	indiceProf, l'indice du professeur
   * 				indiceCours, l'indice du cours enseigne
   */
  def initialiserContraintesProfDonne1SeulCours(indiceProf: Int, indiceCours: Int) {
    for (indice <- List.range(0, listeIntVarFlattened.size)) {
      val boolProf = new BoolVar()
      boolProf <=> (listeIntVarFlattened(indice)(iProf) #= indiceProf)
      val boolCours = new BoolVar()
      boolCours <=> (listeIntVarFlattened(indice)(iCours) #= indiceCours)

      val listeBoolVar = List(boolProf, boolCours)
      OR(sum(listeBoolVar) #= 2, sum(List(boolProf)) #= 0);

    }
  }

  /**
   * Contraint a ce qu'un prof donne uniquement les cours passes en parametre
   * params:	indiceProf, l'indice du professeur
   * 				indiceCours, l'indice du cours enseigne
   * 				indiceCours2, l'indice du cours enseigne
   */
  def initialiserContraintesProfDonne2Cours(indiceProf: Int, indiceCours: Int, indiceCours2: Int) {
    for (indice <- List.range(0, listeIntVarFlattened.size)) {
      val boolProf = new BoolVar()
      boolProf <=> (listeIntVarFlattened(indice)(iProf) #= indiceProf)
      val boolCours = new BoolVar()
      boolCours <=> (listeIntVarFlattened(indice)(iCours) #= indiceCours)
      val boolCours2 = new BoolVar()
      boolCours2 <=> (listeIntVarFlattened(indice)(iCours) #= indiceCours)

      val listeBoolVar = List(boolProf, boolCours)
      val listeBoolVar2 = List(boolProf, boolCours2)
      OR(sum(listeBoolVar) #= 2, sum(listeBoolVar2) #= 2, sum(List(boolProf)) #= 0);

    }
  }

  /**
   * S'assure que chaque trio prof-cours-local soit soit une fourche (toutes les valeurs a 0), soit non (aucunes valeurs a 0)
   */
  def gestionFourches() {
    for (indice <- List.range(0, listeIntVarFlattened.size)) {
      val boolProf = new BoolVar()
      boolProf <=> (listeIntVarFlattened(indice)(iProf) #= 0)
      val boolCours = new BoolVar()
      boolCours <=> (listeIntVarFlattened(indice)(iCours) #= 0)
      val boolLocal = new BoolVar()
      boolLocal <=> (listeIntVarFlattened(indice)(iLocal) #= 0)

      val listeBoolVar = List(boolProf, boolLocal, boolCours)

      OR(sum(listeBoolVar) #= 0, sum(listeBoolVar) #= 3)

    }

  }

  /**
   * Renvoie une liste de BoolVar correspondant aux absences pour un prof avant une heure donnee
   * params: indiceProf, l'indice du professeur
   * 				 indiceHeure, l'indice de l'heure qui indique jusque quand le professeur est absent (indice inclus)
   */
  def absenceProfAvantHeure(indiceProf: Int, indiceHeure: Int): List[BoolVar] = {
    val l = for (serie <- List.range(0, series.size)) yield {
      val liste = for (indice <- List.range(0, indiceHeure * nJours + nTranchesHorairesJour + 1)) yield {
        val bool = new BoolVar()
        bool <=> (listeIntVar(serie)(indice)(iProf) #\= indiceProf)
        bool

      };

      liste
    }
    l.flatten
  }

  /**
   * Renvoie une liste de BoolVar correspondant aux absences pour un prof après une heure donnee
   * params: indiceProf, l'indice du professeur
   * 				 indiceHeure, l'indice de l'heure qui indique a partir de quand le professeur est absent (indice inclus)
   */
  def absenceProfApresHeure(indiceProf: Int, indiceHeure: Int): List[BoolVar] = {

    val l = for (serie <- List.range(0, series.size)) yield {
      val liste =
        for (indice <- List.range(indiceHeure * nJours, nTranchesHorairesSem)) yield {
          val bool = new BoolVar()
          bool <=> (listeIntVar(serie)(indice)(iProf) #\= indiceProf)
          bool
        };
      liste

    }
    l.flatten

  }

  /**
   * Renvoie une liste de BoolVar correspondant aux absences pour un prof a un jour donne
   * params: indiceProf, l'indice du professeur
   * 				 indiceJour, l'indice du jour ou le professeur est absent (0 = lundi, ...)
   */
  def absenceProfJour(indiceProf: Int, indiceJour: Int): List[BoolVar] = {

    val liste = for (indice <- List.range(0, (listeIntVarFlattened.size / 5))) yield {
      val bool = new BoolVar()
      bool <=> (listeIntVarFlattened(indice * nJours + indiceJour)(iProf) #\= indiceProf)
      bool
    };
    liste
  }

  /**
   * place les contraintes d'heures a donner (tranches horaires) pour un professeur donne
   * params: indiceProf, l'indice du professeur
   * 				 nbHeures, le nombre d'heures que le professeur doit donner
   */
  def placerContraintesNombreHeuresADonnerPourProf(indiceProf: Int, nbHeures: Int) {

    val liste: List[BoolVar] =
      for (indice <- List.range(0, listeIntVarFlattened.size)) yield {
        val bool = new BoolVar()
        bool <=> (listeIntVarFlattened(indice)(iProf) #= indiceProf)
        bool
      }
    sum(liste) #= nbHeures
  }

  /**
   * Initialise la map des series pour une annee donnee
   * params:	annee, l'annee du bloc
   * 					nombreSeries, le nombre de series composant le bloc d'etudes
   */
  def initSeries(annee: Int, nombreSeries: Int): Map[String, List[List[IntVar]]] = {
    var map: Map[String, List[List[IntVar]]] = Map.empty;
    for (i <- 1 to nombreSeries) {
      val valeurMap = for (j <- List.range(0, nTranchesHorairesSem)) yield List(
        new IntVar("prof", 0, profs.size),
        new IntVar("cours", 0, cours.size),
        new IntVar("local", 0, locaux.size))
      map += ((annee + "I" + i) -> valeurMap)
    }
    map

  }

}
