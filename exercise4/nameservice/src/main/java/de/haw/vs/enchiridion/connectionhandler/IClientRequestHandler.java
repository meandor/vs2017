package de.haw.vs.enchiridion.connectionhandler;

import java.io.IOException;
import java.io.InputStream;

/**
 * Interface handling incoming Client requests
 */
public interface IClientRequestHandler extends Runnable {

    /**
     * Process the incoming request message
     *
     * @param in InputStream of incoming request
     */
    public void handleIncomingRequest(InputStream in) throws IOException;
}
