package mware_lib.communication;

import de.haw.vs.nameservice.ObjectReference;

import java.net.Socket;

public interface ICommunicationModule {

    /**
     * Invokes a method for a given Object
     * and returns the result the method
     * of the method method
     *
     * @param ref    Alias of the remote object
     * @param method remote function name
     * @param args   Arguments to the remote function
     * @return Object result of the called
     * ObjectReference of String method name Object . . arguments
     */
    Object invoke(ObjectReference ref, String method, Object... args);

    /**
     * Starts the receiver
     */
    void startReceiver();

    /**
     * Handles incoming requests , calls function locally and answers request
     *
     * @param clientSocket Socket from incoming client
     */
    Runnable handleIncomingRequest(Socket clientSocket);
}
