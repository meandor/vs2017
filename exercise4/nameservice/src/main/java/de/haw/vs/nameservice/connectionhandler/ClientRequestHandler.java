package de.haw.vs.nameservice.connectionhandler;

import de.haw.vs.nameservice.NameService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.net.Socket;

public class ClientRequestHandler implements IClientRequestHandler {

    private Socket socket;
    private volatile boolean stopping;
    private NameService nameService;
    private final String shutdownCMD = "QUIT";
    private final Logger log = LoggerFactory.getLogger(ClientRequestHandler.class);
    private ObjectOutputStream output;

    public ClientRequestHandler(Socket socket) {
        this.socket = socket;
        this.nameService = NameService.getInstance();
    }

    public ClientRequestHandler(ObjectOutputStream output) {
        this.output = output;
        this.nameService = NameService.getInstance();
    }

    @Override
    public void handleIncomingRequest(String request) throws IOException {
        if (request.equalsIgnoreCase(this.shutdownCMD)) {
            this.stopping = true;
        } else {
            output.writeObject(this.nameService.resolve(request));
            output.flush();
        }
    }

    @Override
    public void run() {
        InputStream in;
        BufferedReader reader;
        this.output = null;
        String line;

        try {
            in = socket.getInputStream();
            reader = new BufferedReader(new InputStreamReader(in));
            this.output = new ObjectOutputStream(socket.getOutputStream());
            while (!stopping) {
                line = reader.readLine();
                this.handleIncomingRequest(line);
            }
        } catch (IOException e) {
            log.warn("Error during IO Operation", e);
        }


        try {
            socket.close();
        } catch (IOException e) {
            log.warn("Could not close client socket", e);
        }
    }
}
