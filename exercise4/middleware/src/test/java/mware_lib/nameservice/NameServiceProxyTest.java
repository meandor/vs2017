package mware_lib.nameservice;

import de.haw.vs.nameservice.ObjectReference;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;


public class NameServiceProxyTest {

    private NameServiceProxy testee;

    @Before
    public void setUp() throws Exception {
        testee = new NameServiceProxy("localhost", 8888);
        startTestNameService(8888);
    }

    public static void startTestNameService(int port) throws InterruptedException {
        Runnable startServer = () -> {
            TestNameService nameService = new TestNameService(port);
            try {
                nameService.startServer();
            } catch (IOException e) {
                e.printStackTrace();
            }
        };
        new Thread(startServer).start();
        Thread.sleep(500);
    }

    @Test
    public void testRebindObject() throws Exception {
        testee.rebind("TestObject", "zumsel");
        Thread.sleep(500);
    }

    @Test
    public void testResolveObject() throws Exception {
        NameServiceProxy proxy = new NameServiceProxy("localhost", 8888);
        Object expected = new ObjectReference("zumsel", "localhost", 1337);
        Object actual = proxy.resolve("zumsel");
        Thread.sleep(500);
        assertEquals(expected, actual);
    }
}
