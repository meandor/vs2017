package mware_lib.nameservice;

import mware_lib.communication.ReflectionUtil;
import mware_lib.communication.RemoteCall;
import org.junit.Test;
import static org.junit.Assert.*;

public class ReflectionUtilTest {

    private ReflectionUtil testee = new ReflectionUtil();


    @Test
    public void testCall() {
        Object o = new RemoteCall("a","b");
        Object result = testee.call(o,"getAlias");
        assertEquals(result, "a");
    }


}
