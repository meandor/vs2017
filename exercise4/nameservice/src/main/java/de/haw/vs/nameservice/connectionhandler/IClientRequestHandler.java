package de.haw.vs.nameservice.connectionhandler;

import java.io.IOException;

/**
 * Interface handling incoming Client requests
 */
public interface IClientRequestHandler extends Runnable {

    /**
     * Process the incoming request message
     *
     * @param request String of incoming request
     */
    public void handleIncomingRequest(String request) throws IOException;
}
