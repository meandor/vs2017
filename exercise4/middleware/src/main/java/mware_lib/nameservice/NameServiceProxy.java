package mware_lib.nameservice;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.OutputStream;
import java.net.Socket;

public class NameServiceProxy extends NameService {

    private String nameServiceHostName;
    private int nameServicePort;

    /**
     * Constructs the NameServiceProxy.
     *
     * @param nameServiceHostName String Hostname of NameService Server
     * @param nameServicePort     int Port of NameService Server
     */
    public NameServiceProxy(String nameServiceHostName, int nameServicePort) {
        this.nameServiceHostName = nameServiceHostName;
        this.nameServicePort = nameServicePort;
    }


    @Override
    public void rebind(Object servant, String name) {
        try {
            Socket socket = new Socket(nameServiceHostName, nameServicePort);
            OutputStream socketOutputStream = socket.getOutputStream();

            byte[] messageType = new byte[]{NameServiceProtocol.REBIND};
            byte[] alias = NameServiceProtocol.aliasBytes(name);
            byte[] rebindObject = NameServiceProtocol.serializeObject(servant);
            byte[] serializedMessage = new byte[messageType.length + alias.length + rebindObject.length];
            System.arraycopy(messageType, 0, serializedMessage, 0, messageType.length);
            System.arraycopy(alias, 0, serializedMessage, messageType.length, alias.length);
            System.arraycopy(rebindObject, 0, serializedMessage, messageType.length + alias.length, rebindObject.length);

            socketOutputStream.write(serializedMessage);
            socketOutputStream.flush();
        } catch (IOException e) {
            e.printStackTrace();
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
