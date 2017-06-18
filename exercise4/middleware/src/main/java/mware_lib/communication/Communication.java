package mware_lib.communication;

import mware_lib.nameservice.NameService;
import mware_lib.nameservice.NameServiceProtocol;
import mware_lib.nameservice.ObjectReference;

import java.io.*;
import java.net.*;

public class Communication implements ICommunicationModule {
    @Override
    public Object invoke(ObjectReference ref, String method, Object... args) {
        try {
            InetAddress inetAddress = InetAddress.getByName(ref.getHostname());
            Socket socket = new Socket(inetAddress, ref.getPort());
            OutputStream os = socket.getOutputStream();
            os.write(NameServiceProtocol.serializeObject(ref));
            // TODO: message format?
            os.flush();
            ObjectInputStream socketInputStream = new ObjectInputStream(socket.getInputStream());
            return socketInputStream.readObject();
        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
            return null;
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
