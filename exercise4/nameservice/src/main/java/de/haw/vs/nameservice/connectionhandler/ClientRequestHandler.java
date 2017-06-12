package de.haw.vs.nameservice.connectionhandler;

import de.haw.vs.nameservice.NameService;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

public class ClientRequestHandler implements IClientRequestHandler {

    private Socket socket;
    private volatile boolean stopping;
    private NameService nameService;
    private final Logger log = LoggerFactory.getLogger(ClientRequestHandler.class);
    private ObjectOutputStream output;

    public ClientRequestHandler(Socket socket) {
        this.socket = socket;
        this.nameService = NameService.getInstance();
    }

    public ClientRequestHandler(ObjectOutputStream output) {
        this.output = output;
        this.nameService = NameService.getInstance();
    }

    @Override
    public void handleIncomingRequest(byte[] request) throws IOException {
        byte messageType = request[NameServiceProtocol.MSG_TYPE_POSITION];
        switch (messageType) {
            case NameServiceProtocol.SHUTDOWN:
                this.stopping = true;
                break;
            case NameServiceProtocol.REBIND:
                try {
                    String bindAlias = NameServiceProtocol.extractAlias(request);
                    Object bindObject = NameServiceProtocol.extractObject(request);
                    this.nameService.rebind(bindObject, bindAlias);
                } catch (ClassNotFoundException e) {
                    log.warn("Could not parse rebind message properly", e);
                }
                break;
            case NameServiceProtocol.RESOLVE:
                String resolveAlias = NameServiceProtocol.extractAlias(request);
                output.writeObject(this.nameService.resolve(resolveAlias));
                output.flush();
                break;
            default:
                log.warn("Unknown message type received");
                break;
        }
    }

    @Override
    public void run() {
        log.info("Starting Client Thread");
        InputStream in;
        this.output = null;

        try {
            in = socket.getInputStream();
            this.output = new ObjectOutputStream(socket.getOutputStream());
            while (!stopping) {
                this.handleIncomingRequest(IOUtils.toByteArray(in));
            }
        } catch (IOException e) {
            log.warn("Error during IO Operation", e);
        }


        try {
            this.output.close();
            this.socket.close();
        } catch (IOException e) {
            log.warn("Could not close client socket", e);
        }
        log.info("Stopping Client Thread");
    }

    public boolean isStopping() {
        return stopping;
    }
}
