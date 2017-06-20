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
        log.info("Starting Communication Module Receiver");
        try {
            ServerSocket serverSocket = new ServerSocket(port);
            while (!stopping) {
                Socket clientSocket = serverSocket.accept();
                IncomingRequestHandler clientRequest = new IncomingRequestHandler(clientSocket);
                threadPool.submit(clientRequest);
            }
        } catch (IOException e) {
            log.warn("Can not open server socket", e);
        }
        log.info("Stopped Communication Module Receiver");
    }

    public boolean isStopping() {
        return stopping;
    }
}
