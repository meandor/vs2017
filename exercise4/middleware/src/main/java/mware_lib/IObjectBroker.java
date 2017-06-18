package mware_lib;

import mware_lib.nameservice.NameService;

/**
 * Interface for the ObjectBroker.
 * The ObjectBroker should be used to interact with the library.
 */
public interface IObjectBroker {

    /**
     * Returns the NameService (proxy Object).
     *
     * @return NameService
     */
    public NameService getNameService();

    /**
     * Initiates the shutting down sequence for the middleware.
     */
    public void shutDown();
}
