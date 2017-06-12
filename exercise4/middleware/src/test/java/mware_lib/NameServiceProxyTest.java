package mware_lib;

import org.junit.Test;


public class NameServiceProxyTest {
    @Test
    public void testShutdownMessage() {
        NameServiceProxy proxy = new NameServiceProxy("localhost", 8888);
        Object wurst = "abc";
        proxy.rebind(wurst, "zumsel");
        System.out.println(proxy.resolve("zumsel"));
    }
}
