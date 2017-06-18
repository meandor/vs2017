package mware_lib.nameservice;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;

public class TestNameService {

    private int port;
    private ObjectOutput out;
    private final Logger log = LoggerFactory.getLogger(TestNameService.class);

    public TestNameService(int port) {
        this.port = port;
    }

    public void startServer() throws IOException {
        ServerSocket socket = new ServerSocket(this.port);
        log.info("Server started");
        Socket clientSocket = socket.accept();

        InputStream in = clientSocket.getInputStream();
        out = new ObjectOutputStream(clientSocket.getOutputStream());
        this.handleIncomingRequest(IOUtils.toByteArray(in));
        in.close();
        out.close();
        socket.close();
    }

    private void handleIncomingRequest(byte[] request) throws IOException {
        byte messageType = request[NameServiceProtocol.MSG_TYPE_POSITION];
        switch (messageType) {
            case NameServiceProtocol.REBIND:
                try {
                    String bindAlias = NameServiceProtocol.extractAlias(request);
                    ObjectReference bindObject = NameServiceProtocol.extractObject(request);
                    log.info("alias:" + bindAlias);
                    log.info("object:" + bindObject);

                } catch (ClassNotFoundException e) {
                    log.warn("Could not parse rebind message properly", e);
                }
                break;
            case NameServiceProtocol.RESOLVE:
                String resolveAlias = NameServiceProtocol.extractAlias(request);
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
}
