package mware_lib;

/**
 * This class acts as the main entry point for the middleware.
 */
public class ObjectBroker implements IObjectBroker {

    /**
     * Main entry point to the middleware.
     *
     * @param serviceHost String host of NameService
     * @param listenPort  int Port of NameService
     * @param debug       boolean indicates extra logging
     * @return ObjectBroker
     */
    public static ObjectBroker init(String serviceHost, int listenPort, boolean debug) {
        return null;
    }

    /**
     * Returns the NameService (proxy Object).
     *
     * @return NameService
     */
    public NameService getNameService() {
        return null;
    }

    /**
     * Initiates the shutting down sequence for the middleware.
     */
    public void shutDown() {

    }
}