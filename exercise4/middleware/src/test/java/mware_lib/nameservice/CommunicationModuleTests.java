package mware_lib.nameservice;

import de.haw.vs.nameservice.ObjectReference;
import mware_lib.communication.CommunicationModule;
import mware_lib.communication.RemoteCall;
import org.junit.Test;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;

import static org.junit.Assert.*;

public class CommunicationModuleTests {

    private CommunicationModule testee = new CommunicationModule();

    @Test
    public void testInvokeBadCase() {
        // All errors (in this case connection refused) should result in an runtime exception
        Object result = testee.invoke(new ObjectReference("wurst", "localhost", 8018), "fly");
        assertEquals(result.getClass(), RuntimeException.class);
    }

    @Test
    public void testInvokeGoodCase() {
        int port = 8753;
        Runnable echoServer = () -> {
            try {
                ServerSocket serverSocket = new ServerSocket(port);
                Socket socket = serverSocket.accept();
                ObjectInputStream ois = new ObjectInputStream(socket.getInputStream());
                ObjectOutputStream oos = new ObjectOutputStream(socket.getOutputStream());
                Object o = ois.readObject();
                oos.writeObject(o);

                ois.close();
                oos.close();
                socket.close();
                serverSocket.close();
            } catch (Exception e){
                e.printStackTrace();
            }
        };

        new Thread(echoServer).start();

        ObjectReference or = new ObjectReference("wurst", "localhost", port);
        Object result = testee.invoke(or, "fly");
        RemoteCall call = new RemoteCall("wurst","fly");
        assertEquals(call, result);

    }
}
