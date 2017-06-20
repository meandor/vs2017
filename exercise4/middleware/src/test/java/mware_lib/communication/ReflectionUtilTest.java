package mware_lib.communication;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ReflectionUtilTest {

    private ReflectionUtil testee = new ReflectionUtil();


    @Test
    public void testCall() {
        Object o = new RemoteCall("a", "b");
        Object result = testee.call(o, "getAlias");
        assertEquals(result, "a");
    }


}
