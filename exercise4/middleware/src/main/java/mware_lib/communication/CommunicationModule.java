package mware_lib.communication;

import de.haw.vs.nameservice.ObjectReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.Arrays;

public class CommunicationModule implements ICommunication {

    private final Logger logger = LoggerFactory.getLogger(CommunicationModule.class);
    private Thread receiver;

    public CommunicationModule() {
        this.receiver = new Thread(new Receiver());
    }

    @Override
    public Object invoke(ObjectReference ref, String method, Object... args) {
        logger.debug("Starting call: " + ref.getAlias() + " - " + method + " with " + Arrays.toString(args));
        try {
            InetAddress inetAddress = InetAddress.getByName(ref.getHostname());
            Socket socket = new Socket(inetAddress, ref.getPort());

            ObjectOutputStream out = new ObjectOutputStream(socket.getOutputStream());
            ObjectInputStream in = new ObjectInputStream(socket.getInputStream());

            RemoteCall remoteCall = new RemoteCall(ref.getAlias(), method, args);
            out.writeObject(remoteCall);
            out.flush();
            Thread.sleep(200);

            Object result = in.readObject();
            Thread.sleep(200);

            in.close();
            out.close();
            socket.close();
            logger.debug("got result: " + result.toString());
            return result;
        } catch (Exception e) {
            logger.debug("did not get any result", e);
            return new RuntimeException("Method not found");
        }
    }

    @Override
    public void startReceiver() {
        this.receiver.start();
    }

    @Override
    public void shutdown() {
        this.receiver.interrupt();
    }
}
