package de.haw.vs.nameservice.connectionhandler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

public class Server implements IServer {

    private ServerSocket serverSocket;
    private volatile boolean stopping;
    private final Logger log = LoggerFactory.getLogger(Server.class);
    private final int maxConcurrentConnections = 30;
    private final Executor threadPool = Executors.newFixedThreadPool(maxConcurrentConnections);

    @Override
    public void initServer(int port) {
        try {
            serverSocket = new ServerSocket(port);
            this.stopping = false;
        } catch (IOException e) {
            this.stopping = true;
            log.error("Could not start server at port: " + String.valueOf(port), e);
        }
    }

    @Override
    public void run() {
        log.info("Started server");
        while (!stopping) {
            try {
                Socket clientSocket = serverSocket.accept();
                IClientRequestHandler clientRequest = new ClientRequestHandler(clientSocket);
                threadPool.execute(clientRequest);
            } catch (IOException e) {
                log.warn("Error while client tried to connect", e);
            }
        }
        try {
            serverSocket.close();
        } catch (IOException e) {
            log.warn("Could not close server socket");
        }
        log.info("Stopped server");
    }

    public boolean isStopping() {
        return stopping;
    }

    public void stop() {
        this.stopping = true;
    }
}
