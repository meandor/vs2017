package mware_lib.communication;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Receiver implements Runnable {

    private volatile boolean stopping;
    private final int port = 9002;
    private final Logger log = LoggerFactory.getLogger(Receiver.class);
    private final int poolSize = 30;
    private final ExecutorService threadPool = Executors.newFixedThreadPool(poolSize);

    @Override
    public void run() {
        log.debug("Starting Communication Module Receiver");
        try {
            ServerSocket serverSocket = new ServerSocket(port);
            while (!isStopping()) {
                Socket clientSocket = serverSocket.accept();
                log.debug("Got remote request");
                IncomingRequestHandler clientRequest = new IncomingRequestHandler(clientSocket);
                threadPool.submit(clientRequest);
                if (Thread.interrupted()) {
                    this.stopping = true;
                }
            }
            serverSocket.close();
            threadPool.shutdownNow();
        } catch (IOException e) {
            log.warn("Can not open server socket", e);
        }
        log.debug("Stopped Communication Module Receiver");
    }

    public boolean isStopping() {
        return stopping;
    }
}