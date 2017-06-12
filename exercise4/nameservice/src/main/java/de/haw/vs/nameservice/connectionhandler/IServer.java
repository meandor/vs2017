package de.haw.vs.nameservice.connectionhandler;

/**
 * Interface for the NameService Server.
 */
public interface IServer extends Runnable {

    /**
     * Starts the server thread with a socket at the given port
     *
     * @param port int port number
     */
    public void initServer(int port);

    /**
     * Stops the server
     */
    public void stop();
}
