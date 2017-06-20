package mware_lib.nameservice;

import de.haw.vs.nameservice.NameServiceProtocol;
import de.haw.vs.nameservice.ObjectReference;
import mware_lib.NameService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class NameServiceProxy extends NameService {

    private int servicePort = 9002;
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
            ObjectReference ref = new ObjectReference(name, InetAddress.getLocalHost().getHostName(), servicePort);
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
        Object result = null;
        try {
            Socket socket = new Socket(nameServiceHostName, nameServicePort);
            OutputStream out = socket.getOutputStream();
            ObjectInputStream in = new ObjectInputStream(socket.getInputStream());

            byte[] message = NameServiceProtocol.buildResolveMessage(name);

            out.write(message);
            out.flush();
            Thread.sleep(200);
            result = in.readObject();
            Thread.sleep(200);
            out.close();
            in.close();
            socket.close();

        } catch (IOException | ClassNotFoundException | InterruptedException e) {
            log.warn("Something went wrong while trying to resolve: " + name, e);
        }
        return result;
    }

    public Object resolveLocally(String name) {
        return this.localRegistry.getOrDefault(name, null);
    }
}
