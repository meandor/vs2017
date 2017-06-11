package tcp_basics;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;

/**
 * Refresher zur TCP-Programmierung (Clientseite)
 *
 */
public class Client {

	/**
	 * @param args
	 * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {
		Socket mySock;
		BufferedReader in;
		OutputStream out;		
		
		// Verbindung aufbauaen
		mySock = new Socket("localhost", 14001);
		
		// I/O-Kanäle der Socket
		in = new BufferedReader(new InputStreamReader(mySock.getInputStream()));
		out = mySock.getOutputStream();
		
		// Kommunikation
		out.write(("Knock, knock!\n").getBytes());
		System.out.println(in.readLine());
		
		// Verbindung schliessen
		in.close();
		out.close();
		mySock.close();
	}

}
