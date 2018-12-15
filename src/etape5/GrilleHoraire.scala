package etape5

import JaCoP.scala._
import scala.reflect.ClassManifestFactory.classType
import scala.collection.mutable.ListBuffer

/**
 * contraintes pour l etape 4 :
 *  - M. Grolaux donne 20 heures de cours (=10 tranches horaires de 2h)
 *  - M. Grolaux ne souhaite pas donné cours avant 10h30 et après 13h00
 *  - M.Grolaux ne donne pas cours le lundi a la serie 1
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
    4 -> "Leleux")

  val cours1BINTh = Map(
    1 -> "APOO (th)", 2 -> "Algo 1 (th)", 3 -> "DO", 4 -> "Anglais 1", 5 -> "Compta", 6 -> "Eco", 7 -> "Math 1 (th)")

    val cours1BINEx = Map(
    1 -> "APOO (ex)", 2 -> "Algo 1 (ex)", 3 -> "Math 1 (ex)")

 
  val cours = Map(
    1 -> "Infra",
    2 -> "PLFC",
    3 -> "Web3",
    4 -> "Pattern")

  val locaux = Map(
    1 -> "AudA",
    2 -> "AudB",
    3 -> "B11",
    4 -> "B12",
    5 -> "B21",
    7 -> "Labo")

  // nombre profs/cours/locaux/series/jours/horaires
  val nProf = 4
  val nCours = 4
  val nLocaux = 2
  val nSeries = 2
  val nJours = 5
  val nTranchesHorairesJour = 4
  val nTranchesHorairesCours = 2
  val nTranchesHorairesSem = nJours * nTranchesHorairesJour

  val series = initSeries(1, 4) ++ initSeries(2, 3) ++ initSeries(3, 2)
  val listeIntVar = series.values.toList;
  val listeIntVarFlattened = listeIntVar.flatten

  /* -----------
   * CONTRAINTES
   * -----------
   */

  /* COURS */
  val contraintesSoftDamas = absenceProfJour(2, 0)
  sum(contraintesSoftDamas, contraintesSoftDamas.size)

  val contraintesSoftChoquet = absenceProfApresHeure(3, 2)
  sum(contraintesSoftChoquet, contraintesSoftChoquet.size)

  val contraintesSoftGrolaux = absenceProfAvantHeure(1, 0)
  sum(contraintesSoftGrolaux, contraintesSoftGrolaux.size)
  //TODO gerer soft

  // - M. Grolaux donne 20 heures de cours
  val contraintesObligatoiresGrolaux = placerContraintesNombreHeuresADonnerPourProf(1, 20)

  // -M. Grolaux ne souhaite pas donné cours avant 10h30 et après 13h00
  /*val contraintesSoftsGrolaux= absenceProfAvantHeure(1, 0)::: //contraintes grolaux enonce
                         absenceProfApresHeure(1, 2)

   val contraintesSoft=count(contraintesSoftsGrolaux,0)



  /* TRANCHES HORAIRES */
  for (i <- List.range(0, nTranchesHorairesSem)) {
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

  }*/

  /* --------
   * RESEARCH
   * --------
   */
  val all_series = series.values.flatten.toList.flatten

  //val mySearch = search(all_series, first_fail, indomain_middle)
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

  /*def absenceProfJourHeure(indiceProf:Int,indiceJour:Int,indiceHeure:Int) : List[BoolVar] = {
    val indiceTrancheHoraire=indiceHeure*5+indiceJour
    val bool=new BoolVar()
    val bool2=new BoolVar()
    bool<=> (serie1(indiceTrancheHoraire)(iProf) #\= indiceProf)
    bool2<=>(serie2(indiceTrancheHoraire)(iProf) #\= indiceProf)
    List(bool,bool2)
  }
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

  def absenceProfJour(indiceProf: Int, indiceJour: Int): List[BoolVar] = {
    print(listeIntVarFlattened.size)

    val liste = for (indice <- List.range(0, (listeIntVarFlattened.size / 5))) yield {
      val bool = new BoolVar()
      bool <=> (listeIntVarFlattened(indice * nJours + indiceJour)(iProf) #\= indiceProf)
      bool
    };
    liste
  }

  
  def placerContraintesLocaux1BIN(){
    for (tc <- List.range(0, nTranchesHorairesSem)) yield {
      
      
    }
    
    
  }
  def placerContraintesNombreHeuresADonnerPourProf(indiceProf: Int, nbHeures: Int) {

    val liste: List[BoolVar] =
      for (indice <- List.range(0, listeIntVarFlattened.size)) yield {
        val bool = new BoolVar()
        bool <=> (listeIntVarFlattened(indice)(iProf) #= indiceProf)
        bool
      }
    sum(liste) #= nbHeures
  }

  def append[A](x: List[A], y: List[A]): List[A] = for (e <- x.++(y)) yield e

  def placerContraintesNombreHeuresCours(indiceCours: Int, nbHeures: Int) {
    series.foreach((e: (String, List[List[IntVar]])) => {
      val liste = for (indice <- List.range(0, nTranchesHorairesSem)) yield {
        val bool = new BoolVar()
        bool <=> (e._2(indice)(iCours) #= indiceCours)
        bool
      };
      sum(liste) #= nbHeures
    })

  }

  def initSeries(annee: Int, nombreSeries: Int): Map[String, List[List[IntVar]]] = {
    var map: Map[String, List[List[IntVar]]] = Map.empty;
    for (i <- 1 to nombreSeries) {
      val valeurMap = for (j <- List.range(0, nTranchesHorairesSem)) yield List(
        new IntVar("prof", 0, nProf),
        new IntVar("cours", 0, nCours),
        new IntVar("local", 0, nLocaux))
      map += ((annee + "I" + i) -> valeurMap)
    }
    map

  }

}
