package de.haw.vs.nameservice.connectionhandler;

/**
 * Interface handling incoming Client requests
 */
public interface IClientRequestHandler extends Runnable {

    /**
     * Process the incoming request message
     *
     * @param request String of incoming request
     */
    public void handleIncomingRequest(String request);
}
