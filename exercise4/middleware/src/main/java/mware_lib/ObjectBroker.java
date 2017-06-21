package mware_lib;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import de.haw.vs.nameservice.ObjectReference;
import mware_lib.communication.CommunicationModule;
import mware_lib.communication.ReflectionUtil;
import mware_lib.nameservice.NameServiceProxy;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;

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
            Logger root = (Logger) LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME);
            root.setLevel(Level.DEBUG);
            instance = new ObjectBroker(serviceHost, listenPort);
        }
        return instance;
    }

    private ObjectBroker(String serviceHost, int listenPort) {
        logger.debug("Starting ORB");
        this.nameService = new NameServiceProxy(serviceHost, listenPort);
        logger.debug("Binding NameService");
        this.communicationModule = new CommunicationModule();
        logger.debug("Started CommunicationModule");
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
        logger.debug("Initiating Shutdown");
        this.communicationModule.shutdown();
        instance = null;
    }

    public Object remoteCall(ObjectReference ref, String methodName, Object... args) {
        return communicationModule.invoke(ref, methodName, args);
    }

    public Object localCall(ObjectReference ref, String methodName, Object... args) {
        Object resolved = ((NameServiceProxy) nameService).resolveLocally(ref.getAlias());
        Object result = ReflectionUtil.call(resolved, methodName, args);
        if (result.getClass() == InvocationTargetException.class) {
            InvocationTargetException e = (InvocationTargetException) result;
            return e.getCause();
        } else {
            return result;
        }
    }
}