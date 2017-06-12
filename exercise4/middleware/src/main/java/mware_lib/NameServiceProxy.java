package mware_lib;

import java.io.*;
import java.net.Socket;

public class NameServiceProxy extends NameService {

    private String nameServiceHostName;
    private int nameServicePort;



    /**
     * Initialises the NameService stub. The actual name service can run anywhere on a given host/port
     *
     * @param nameServiceHostName The hostname of the remote name service
     * @param nameServicePort     The port of the remote name service
     */
    public NameServiceProxy(String nameServiceHostName, int nameServicePort) {
        this.nameServiceHostName = nameServiceHostName;
        this.nameServicePort = nameServicePort;
    }



    @Override
    public void rebind(Object servant, String name) {
        try(Socket stubSocket = new Socket(nameServiceHostName, nameServicePort);
            OutputStream socketOutputStream = stubSocket.getOutputStream()) {
            byte[] serializedMessage = NameServiceRequestSerializer.serializeRebindMessage(servant, name);
            socketOutputStream.write(serializedMessage);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    @Override
    public Object resolve(String name) {
        try(Socket stubSocket = new Socket(nameServiceHostName, nameServicePort);
            OutputStream socketOutputStream = stubSocket.getOutputStream();
            ObjectInputStream socketInputStream =  new ObjectInputStream(stubSocket.getInputStream())) {
            byte[] serializedMessage = NameServiceRequestSerializer.serializeResolveMessage(name);
            socketOutputStream.write(serializedMessage);
            return socketInputStream.readObject();
        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
            return null;
        }
    }
}
