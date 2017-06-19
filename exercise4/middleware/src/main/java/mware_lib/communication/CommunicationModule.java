package mware_lib.communication;

import de.haw.vs.nameservice.NameServiceProtocol;
import de.haw.vs.nameservice.ObjectReference;

import java.io.*;
import java.net.InetAddress;
import java.net.Socket;

public class CommunicationModule implements ICommunication {

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

        } catch (IOException | InterruptedException | ClassNotFoundException e) {
            e.printStackTrace();
            return new RuntimeException("Method not found");
        }
    }

    @Override
    public void startReceiver() {

    }

    @Override
    public Runnable handleIncomingRequest(Socket clientSocket) {
        return null;
    }

}
