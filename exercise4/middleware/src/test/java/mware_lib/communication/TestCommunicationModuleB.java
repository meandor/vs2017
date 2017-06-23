package mware_lib.communication;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;

public class TestCommunicationModuleB implements Runnable {

    private int port;
    private Logger logger = LoggerFactory.getLogger(TestCommunicationModuleB.class);

    public TestCommunicationModuleB(int port) {
        this.port = port;
    }

    @Override
    public void run() {
        try {
            ServerSocket socket = new ServerSocket(this.port);
            logger.info("Comm B started");
            Socket clientSocket = socket.accept();

            ObjectInputStream in = new ObjectInputStream(clientSocket.getInputStream());
            ObjectOutputStream out = new ObjectOutputStream(clientSocket.getOutputStream());

            if (in.readObject().equals(new RemoteCall("zumsel", "foobar", "boo", "baz"))) {
                logger.info("Got correct remote call");
                out.writeObject("over 9000");
            } else {
                logger.info("Got wrong remote call");
                out.writeObject(new RuntimeException("Method not found"));
            }
            Thread.sleep(20);
            in.close();
            out.close();
            clientSocket.close();
            socket.close();
        } catch (IOException | ClassNotFoundException e) {
            logger.warn("zish", e);
        } catch (InterruptedException e) {
            logger.info("Stopped Comm B");
        }
    }
}
