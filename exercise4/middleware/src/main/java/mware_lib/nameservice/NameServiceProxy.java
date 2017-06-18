package mware_lib.nameservice;

import de.haw.vs.nameservice.NameServiceProtocol;
import de.haw.vs.nameservice.ObjectReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class NameServiceProxy extends NameService {

    private String nameServiceHostName;
    private int nameServicePort;
    private ConcurrentMap<String, Object> localRegistry;
    private final Logger log = LoggerFactory.getLogger(NameServiceProxy.class);

    /**
     * Constructs the NameServiceProxy.
     *
     * @param nameServiceHostName String Hostname of NameService Server
     * @param nameServicePort     int Port of NameService Server
     */
    public NameServiceProxy(String nameServiceHostName, int nameServicePort) {
        this.nameServiceHostName = nameServiceHostName;
        this.nameServicePort = nameServicePort;
        this.localRegistry = new ConcurrentHashMap<>();
    }

    @Override
    public void rebind(Object servant, String name) {
        try {
            Socket socket = new Socket(nameServiceHostName, nameServicePort);
            OutputStream out = socket.getOutputStream();
            ObjectReference ref = new ObjectReference(name, this.nameServiceHostName, this.nameServicePort);
            byte[] message = NameServiceProtocol.buildRebindMessage(ref, name);
            out.write(message);
            out.flush();
            Thread.sleep(200);
            out.close();
            socket.close();
            this.localRegistry.put(name, servant);
        } catch (IOException e) {
            log.warn("Message could not be send", e);
        } catch (InterruptedException e) {
            log.warn("Was not able to wait before closing socket", e);
        }
    }

    @Override
    public Object resolve(String name) {
        try {
            Socket socket = new Socket(nameServiceHostName, nameServicePort);
            OutputStream socketOutputStream = socket.getOutputStream();
            ObjectInputStream socketInputStream = new ObjectInputStream(socket.getInputStream());

            byte[] messageType = new byte[]{NameServiceProtocol.RESOLVE};
            byte[] alias = NameServiceProtocol.aliasBytes(name);
            byte[] serializedMessage = new byte[messageType.length + alias.length];
            System.arraycopy(messageType, 0, serializedMessage, 0, messageType.length);
            System.arraycopy(alias, 0, serializedMessage, messageType.length, alias.length);

            socketOutputStream.write(serializedMessage);
            socketOutputStream.flush();
            return socketInputStream.readObject();
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}
