package de.haw.vs.enchiridion.connectionhandler;

import de.haw.vs.enchiridion.NameService;
import de.haw.vs.nameservice.NameServiceProtocol;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ClientRequestHandler implements IClientRequestHandler {

    private Socket socket;
    private NameService nameService;
    private ObjectOutputStream output;
    private final Logger log = LoggerFactory.getLogger(ClientRequestHandler.class);
    private final int clientTimeout = 10000;

    public ClientRequestHandler(Socket socket) {
        this.socket = socket;
        this.nameService = NameService.getInstance();
    }

    public ClientRequestHandler(ObjectOutputStream output) {
        this.output = output;
        this.nameService = NameService.getInstance();
    }

    @Override
    public void handleIncomingRequest(InputStream in) throws IOException {
        int nextByteOfData;
        byte messageType;

        List<Byte> fullMessage = new ArrayList<>();
        while ((nextByteOfData = in.read()) != NameServiceProtocol.END_OF_MESSAGE_INT) {
            fullMessage.add((byte) nextByteOfData);
        }

        log.info(String.valueOf(fullMessage));

        byte[] request = this.toByteArray(fullMessage);
        log.info("array: " + Arrays.toString(request));
        messageType = request[NameServiceProtocol.MSG_TYPE_POSITION];
        log.info("message-type byte:" + messageType);
        String alias = NameServiceProtocol.extractAlias(request);
        log.info(alias);
        switch (messageType) {
            case NameServiceProtocol.REBIND:
                log.info("Rebinding");
                try {
                    this.nameService.rebind(NameServiceProtocol.extractObject(request), alias);
                } catch (ClassNotFoundException e) {
                    log.warn("Could not parse rebind message properly", e);
                }
                break;
            case NameServiceProtocol.RESOLVE:
                log.info("Resolving");
                output.writeObject(this.nameService.resolve(alias));
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
            this.socket.setSoTimeout(this.clientTimeout);
            in = socket.getInputStream();
            this.output = new ObjectOutputStream(socket.getOutputStream());
            this.handleIncomingRequest(in);
        } catch (SocketException e) {
            log.warn("Client took too long", e);
        } catch (IOException e) {
            log.warn("Error during IO Operation", e);
        }

        try {
            Thread.sleep(200);
            this.output.close();
            this.socket.close();
        } catch (IOException e) {
            log.warn("Could not close client socket", e);
        } catch (InterruptedException e) {
            log.warn("Could not wait before closing socket", e);
        }
        log.info("Stopping Client Thread");
    }

    private byte[] toByteArray(List<Byte> list) {
        log.info("Starting to byte array");
        byte[] result = new byte[list.size()];
        for (int i = 0; i < list.size(); i++) {
            result[i] = list.get(i);
        }
        log.info("Finished to byte array");
        return result;
    }
}
