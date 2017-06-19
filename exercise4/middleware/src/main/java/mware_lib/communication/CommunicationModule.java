package mware_lib.communication;

import de.haw.vs.nameservice.ObjectReference;

import java.net.Socket;

public class CommunicationModule implements ICommunication {

    @Override
    public Object invoke(ObjectReference ref, String method, Object... args) {
        return null;
    }

    @Override
    public void startReceiver() {

    }

    @Override
    public Runnable handleIncomingRequest(Socket clientSocket) {
        return null;
    }
}
