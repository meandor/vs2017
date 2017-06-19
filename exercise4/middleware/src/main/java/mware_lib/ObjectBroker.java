package mware_lib;

import de.haw.vs.nameservice.ObjectReference;
import mware_lib.communication.CommunicationModule;
import mware_lib.nameservice.NameServiceProxy;
import ch.qos.logback.classic.Logger;
import org.slf4j.LoggerFactory;
import ch.qos.logback.classic.Level;

/**
 * This class acts as the main entry point for the middleware.
 */
public class ObjectBroker implements IObjectBroker {

    private NameService nameService;
    private CommunicationModule communicationModule;
    private static ObjectBroker instance;
    private final Logger logger = (Logger) LoggerFactory.getLogger(ObjectBroker.class);

    /**
     * Main entry point to the middleware.
     *
     * @param serviceHost String host of NameService
     * @param listenPort  int Port of NameService
     * @param debug       boolean indicates extra logging
     * @return ObjectBroker
     */
    public static ObjectBroker init(String serviceHost, int listenPort, boolean debug) {
        if (instance == null) {
            instance = new ObjectBroker(serviceHost, listenPort, debug);
            if(debug) {
                Logger root = (Logger)LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME);
                root.setLevel(Level.DEBUG);
            }
        }
        return instance;
    }

    private ObjectBroker(String serviceHost, int listenPort, boolean debug) {
        this.nameService = new NameServiceProxy(serviceHost, listenPort);
        this.communicationModule = new CommunicationModule();
        this.communicationModule.startReceiver();
    }


    /**
     * Returns the NameService (proxy Object).
     *
     * @return NameService
     */
    public NameService getNameService() {
        return this.nameService;
    }

    /**
     * Initiates the shutting down sequence for the middleware.
     */
    public void shutDown() {

    }

    public Object remoteCall(ObjectReference ref, String methodName, Object... args) {
        return communicationModule.invoke(ref, methodName, args);
    }

    public Object localCall(ObjectReference ref, String methodName, Object... args) {
        return null;
    }
}