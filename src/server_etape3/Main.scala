package server_etape3
import JaCoP.scala._
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import org.eclipse.jetty.servlet.ServletHolder
import org.eclipse.jetty.servlet.DefaultServlet

object Main extends jacop{
  def main(args: Array[String]): Unit = {
    val server= new Server(8080);
    val context= new WebAppContext();
    
    context.setResourceBase("www");
		context.addServlet(new ServletHolder(new DefaultServlet()), "/");
		context.addServlet(new ServletHolder(HoraireServlet), "/getHoraire");
		
		server.setHandler(context);
		server.start();
    
  }
}