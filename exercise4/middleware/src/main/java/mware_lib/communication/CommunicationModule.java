package mware_lib.communication;

import ch.qos.logback.classic.Logger;
import de.haw.vs.nameservice.ObjectReference;
import org.slf4j.LoggerFactory;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetAddress;
import java.net.Socket;

public class CommunicationModule implements ICommunication {

    private final Logger logger = (Logger) LoggerFactory.getLogger(CommunicationModule.class);

    @Override
    public Object invoke(ObjectReference ref, String method, Object... args) {
        try {

            InetAddress inetAddress = InetAddress.getByName(ref.getHostname());
            Socket socket = new Socket(inetAddress, ref.getPort());

            ObjectOutputStream socketOutputStream = new ObjectOutputStream(socket.getOutputStream());
            ObjectInputStream socketInputStream = new ObjectInputStream(socket.getInputStream());

            RemoteCall remoteCall = new RemoteCall(ref.getAlias(), method, args);
            socketOutputStream.writeObject(remoteCall);
            socketOutputStream.flush();

            Object result = socketInputStream.readObject();
            Thread.sleep(200);

            socketInputStream.close();
            socketOutputStream.close();
            socket.close();

            return result;

        } catch (Exception e) {
            logger.debug(e.toString());
            return new RuntimeException("Method not found");
        }
    }

    @Override
    public void startReceiver() {
        new Thread(new Receiver()).start();
    }
}
