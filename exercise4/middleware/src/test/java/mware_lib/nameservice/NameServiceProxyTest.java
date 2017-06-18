package mware_lib.nameservice;

import org.junit.Before;
import org.junit.Test;

import java.io.IOException;


public class NameServiceProxyTest {

    @Before
    public void setUp() throws Exception {
        Runnable startServer = () -> {
            TestNameService nameService = new TestNameService(8888);
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
        NameServiceProxy proxy = new NameServiceProxy("localhost", 8888);
        Object wurst = "abc";
        proxy.rebind(wurst, "zumsel");
        Thread.sleep(500);
//        System.out.println(proxy.resolve("zumsel"));
    }
}
