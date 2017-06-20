package mware_lib.communication;

import de.haw.vs.nameservice.ObjectReference;

public interface ICommunication {

    /**
     * Invokes a method for a given remote Object and returns the result of the method
     *
     * @param ref    Alias of the remote object
     * @param method remote function name
     * @param args   Arguments to the remote function
     * @return Object result of the called ObjectReference of String method name Object . . arguments
     */
    Object invoke(ObjectReference ref, String method, Object... args);

    /**
     * Starts the receiver
     */
    void startReceiver();
}
