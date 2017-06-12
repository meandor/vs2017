package de.haw.vs.nameservice.connectionhandler;

import de.haw.vs.nameservice.NameService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.Socket;

public class ClientRequestHandler implements IClientRequestHandler {

    private Socket socket;
    private volatile boolean stopping;
    private NameService nameService;
    private final String shutdownCMD = "QUIT";
    private final Logger log = LoggerFactory.getLogger(ClientRequestHandler.class);

    public ClientRequestHandler(Socket socket) {
        this.socket = socket;
        this.nameService = NameService.getInstance();
    }

    @Override
    public void handleIncomingRequest(String request) {
        if (request.equalsIgnoreCase(this.shutdownCMD)) {
            this.stopping = true;
        } else {

        }
    }

    @Override
    public void run() {
        while (!stopping) {

        }

        try {
            socket.close();
        } catch (IOException e) {
            log.warn("Could not close client socket", e);
        }
    }
}
