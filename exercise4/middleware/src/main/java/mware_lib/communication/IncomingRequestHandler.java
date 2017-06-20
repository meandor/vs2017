package mware_lib.communication;

import de.haw.vs.nameservice.ObjectReference;
import mware_lib.ObjectBroker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

/**
 * Handles incoming requests, calls function locally and answers request
 */
public class IncomingRequestHandler implements Runnable {

    private Socket clientSocket;
    private Logger logger = LoggerFactory.getLogger(IncomingRequestHandler.class);

    public IncomingRequestHandler(Socket clientSocket) {
        this.clientSocket = clientSocket;
    }

    @Override
    public void run() {
        logger.debug("Start handling request");
        try {
            ObjectInputStream in = new ObjectInputStream(clientSocket.getInputStream());
            ObjectOutputStream out = new ObjectOutputStream(clientSocket.getOutputStream());

            RemoteCall call = (RemoteCall) in.readObject();
            logger.debug("Found RemoteCall");
            Thread.sleep(200);

            ObjectBroker orb = ObjectBroker.init("alreadyExists", 0, false);
            Object result = orb.localCall(new ObjectReference(call.getAlias(), "nonsense", 0), call.getMethodName(), call.getArgs());
            logger.debug("Start sending result");
            out.writeObject(result);
            logger.debug("Result send");
            Thread.sleep(200);
            in.close();
            out.close();
            clientSocket.close();
        } catch (IOException | ClassNotFoundException | InterruptedException e) {
            e.printStackTrace();
        }
        logger.debug("Finished handling request");
    }
}
