package mware_lib.nameservice;

import de.haw.vs.nameservice.NameServiceProtocol;
import de.haw.vs.nameservice.ObjectReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

class TestNameService {

    private int port;
    private ObjectOutput out;
    private final Logger log = LoggerFactory.getLogger(TestNameService.class);

    TestNameService(int port) {
        this.port = port;
    }

    void startServer() throws IOException {
        ServerSocket socket = new ServerSocket(this.port);
        log.info("Server started");
        Socket clientSocket = socket.accept();

        InputStream in = clientSocket.getInputStream();
        out = new ObjectOutputStream(clientSocket.getOutputStream());

        this.handleIncomingRequest(in);
        clientSocket.close();
        in.close();
        out.close();
        socket.close();
    }

    private void handleIncomingRequest(InputStream in) throws IOException {
        int nextByteOfData;
        byte messageType;

        List<Byte> fullMessage = new ArrayList<>();
        while ((nextByteOfData = in.read()) != NameServiceProtocol.END_OF_MESSAGE_INT) {
            fullMessage.add((byte) nextByteOfData);
        }

        byte[] messageBytes = this.toByteArray(fullMessage);
        messageType = messageBytes[NameServiceProtocol.MSG_TYPE_POSITION];

        switch (messageType) {
            case NameServiceProtocol.REBIND:
                log.info("Rebind");
                try {
                    String bindAlias = NameServiceProtocol.extractAlias(messageBytes);
                    ObjectReference bindObject = NameServiceProtocol.extractObject(messageBytes);
                    log.info("alias:" + bindAlias);
                    log.info("object:" + bindObject);
                } catch (ClassNotFoundException e) {
                    log.warn("Could not parse rebind message properly", e);
                }
                break;
            case NameServiceProtocol.RESOLVE:
                log.info("Resolve");
                String resolveAlias = NameServiceProtocol.extractAlias(messageBytes);
                log.info("alias:" + resolveAlias);
                Object resolved = new ObjectReference("zumsel", "localhost", 1337);
                out.writeObject(resolved);
                out.flush();
                break;
            default:
                log.warn("Unknown message type received");
                break;
        }
    }

    private byte[] toByteArray(List<Byte> list) {
        byte[] result = new byte[list.size()];
        for (int i = 0; i < list.size(); i++) {
            result[i] = list.get(i);
        }
        return result;
    }
}
