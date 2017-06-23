package mware_lib.communication;

import mware_lib.IObjectBroker;
import mware_lib.ObjectBroker;
import mware_lib.nameservice.NameServiceProxyTest;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

import static org.junit.Assert.assertEquals;

public class ReceiverTest {

    private Receiver testee;
    private IObjectBroker orb;

    @Before
    public void setUp() throws Exception {
        this.testee = new Receiver();
        NameServiceProxyTest.startTestNameService(8890);
        this.orb = ObjectBroker.init("localhost", 8890, false);
        Thread.sleep(200);
    }

    @After
    public void tearDown() throws Exception {
        this.orb.shutDown();
        Thread.sleep(400);
    }

    @Test
    public void localCallForNullReceived() throws Exception {
        Socket clientSocket = new Socket("localhost", 9002);
        ObjectOutputStream out = new ObjectOutputStream(clientSocket.getOutputStream());
        ObjectInputStream in = new ObjectInputStream(clientSocket.getInputStream());

        out.writeObject(new RemoteCall("foobar", "doFoo"));
        out.flush();
        Object result = in.readObject();
        assertEquals(NullPointerException.class, result.getClass());
        out.close();
        in.close();
        clientSocket.close();
    }

    @Test
    public void localSuccessfulCallReceived() throws Exception {
        this.orb.getNameService().rebind("foobar", "foobar");
        Socket clientSocket = new Socket("localhost", 9002);
        ObjectOutputStream out = new ObjectOutputStream(clientSocket.getOutputStream());
        ObjectInputStream in = new ObjectInputStream(clientSocket.getInputStream());

        out.writeObject(new RemoteCall("foobar", "length"));
        out.flush();
        Object result = in.readObject();
        assertEquals("foobar".length(), result);
    }
}
