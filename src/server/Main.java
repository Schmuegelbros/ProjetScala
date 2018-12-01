package server;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.DefaultServlet;
import org.eclipse.jetty.servlet.ServletHolder;
import org.eclipse.jetty.webapp.WebAppContext;

public class Main {
	public static void main(String[] args) throws Exception {
		Server server = new Server(8080);
		WebAppContext context= new WebAppContext();
		
		context.setResourceBase("www");
		context.addServlet(new ServletHolder(new DefaultServlet()), "/");
		context.addServlet(new ServletHolder(new HoraireServlet()), "/getHoraire");
		
		
		server.setHandler(context);
		server.start();
		
	}
}
