package mware_lib.communication;

import mware_lib.nameservice.ObjectReference;

import java.io.*;
import java.net.*;

public class Communication implements ICommunicationModule {
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
