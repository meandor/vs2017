package mware_lib;

import de.haw.vs.nameservice.ObjectReference;
import mware_lib.communication.Communication;
import mware_lib.nameservice.NameServiceProxy;

/**
 * This class acts as the main entry point for the middleware.
 */
public class ObjectBroker implements IObjectBroker {

    private NameService nameService;
    private Communication communication;

    /**
     * Main entry point to the middleware.
     *
     * @param serviceHost String host of NameService
     * @param listenPort  int Port of NameService
     * @param debug       boolean indicates extra logging
     * @return ObjectBroker
     */
    public static ObjectBroker init(String serviceHost, int listenPort, boolean debug) {
        return new ObjectBroker(serviceHost, listenPort, debug);
    }

    private ObjectBroker(String serviceHost, int listenPort, boolean debug) {
        this.nameService = new NameServiceProxy(serviceHost, listenPort);
        this.communication = new Communication();
        this.communication.startReceiver();
    }


    /**
     * Returns the NameService (proxy Object).
     *
     * @return NameService
     */
    public NameService getNameService() {
        return nameService;
    }

    /**
     * Initiates the shutting down sequence for the middleware.
     */
    public void shutDown() {

    }

    public Object remoteCall(ObjectReference alias, String methodName, Object... args) {
        return communication.invoke(alias, methodName, args);
    }
}