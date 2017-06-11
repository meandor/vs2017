package tcp_basics;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * Refresher zur TCP-Programmierung (Serverseite)
 *
 */
public class Server {
	
	/**
	 * @param args
	 * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {
		ServerSocket mySvrSocket;
		BufferedReader in;
		OutputStream out;		
		
		mySvrSocket = new ServerSocket(14001);		
		
		// Auf Verbindungsanfrage warten.
		Socket mySock = mySvrSocket.accept(); // -> Socket fuer die eigentliche Verbindung
		
		// I/O-Kanäle der Socket
		in = new BufferedReader(new InputStreamReader(mySock.getInputStream()));
		out = mySock.getOutputStream();
		
		// Kommunikation
		System.out.println(in.readLine());
		out.write(("Who's there?\n").getBytes());
		
		// Verbindung schliessen
		in.close();
		out.close();
		mySock.close();
		
		// Server runterfahren
		mySvrSocket.close();
	}
}
