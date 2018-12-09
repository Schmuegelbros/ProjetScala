package server_etape3

import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import spray.json._
import DefaultJsonProtocol._


object HoraireServlet extends HttpServlet {
  val grille=GrilleHoraire;
  val horaire=grille.horaire();
  
  override def doGet(req:HttpServletRequest, resp:HttpServletResponse){
    val json=horaire.toJson;
    //println(json.toString());
		resp.setContentType("application/json");
		resp.getOutputStream().write(json.toString().getBytes);
  }
}