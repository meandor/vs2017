package mware_lib.communication;

import java.net.Socket;

/**
 * Handles incoming requests , calls function locally and answers request
 */
public class IncomingRequestHandler implements Runnable {

    private Socket clientSocket;

    public IncomingRequestHandler(Socket clientSocket) {
        this.clientSocket = clientSocket;
    }

    @Override
    public void run() {

    }
}
