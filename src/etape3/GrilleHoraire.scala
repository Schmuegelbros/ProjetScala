

package etape3

import JaCoP.scala._
import scala.reflect.ClassManifestFactory.classType

/**
 * contraintes pour l etape 3 : 
 * 	- Chaque cours n est donne que 2 fois par serie et par semaine
 *  - M. Grolaux, M. Choquet et M. Damas ne donne pas cours avant 10h45
 *  - Aucun prof ne donne cours le lundi
 *  - Chaque serie a cours dans un local et avec un prof different 
 */
object GrilleHoraire extends App with jacop {

  def void():Unit={
    
  }
  
  def horaire(): Map[String,List[List[String]]]={
      /*	--------------
   *  INITIALISATION
   *  --------------
   */
  
  //indices
  val iProf = 0
  val iCours = 1
  val iLocal = 2

  val series = Map(
  0 -> "serie 1",
  1 -> "serie 2")
  
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
    sum(coursTempS1) #= 2 // 2 tranche horaires pour chaque cours par serie
    sum(coursTempS2) #= 2 // 2 tranche horaires pour chaque cours par serie
  }
  
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
      for(k<- 1 to profs.size){
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
    val result = satisfy(mySearch,void)
    
    var json: Map[String,List[List[String]]]=Map.empty;
    for(i <- 0 to nSeries-1){
      var liste=for (j <- List.range(0, nTranchesHorairesSem)) yield{
        val indiceP=(j*3+iProf)+(i*nTranchesHorairesSem*3);
        val indiceC=(j*3+iCours)+(i*nTranchesHorairesSem*3);
        val indiceL=(j*3+iLocal)+(i*nTranchesHorairesSem*3);
        List(  
            profs.get(all_series(indiceP).value()) match {
              case Some(name) => name
              case None => ""
            },
            cours.get(all_series(indiceC).value()) match {
              case Some(name) => name
              case None => ""
            },
            locaux.get(all_series(indiceL).value()) match {
              case Some(name) => name
              case None => ""
            }          
        );
      }
      //.+ ne fonctionne pas car n ajoute pas dans la map
      json += (series.get(i).get->liste)
    }
    
    json
  }
  
}

