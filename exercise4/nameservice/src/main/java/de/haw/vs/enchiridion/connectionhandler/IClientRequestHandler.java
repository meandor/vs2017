package de.haw.vs.enchiridion.connectionhandler;

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
    public void handleIncomingRequest(byte[] request) throws IOException;
}
