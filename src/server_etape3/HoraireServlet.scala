package server_etape3

import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import com.owlike.genson.Genson


object HoraireServlet extends HttpServlet {
  val grille=GrilleHoraire;
  val horaire=grille.horaire();
  
  override def doGet(req:HttpServletRequest, resp:HttpServletResponse){
		println(req);
		println(horaire);
    val json=new Genson().serialize("hello world");
		resp.setContentType("application/json");
		resp.getOutputStream().write(json.getBytes());
  }
}