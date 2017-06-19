package mware_lib.nameservice;

import mware_lib.communication.ReflectionUtil;
import org.junit.Test;
import static org.junit.Assert.*;

public class ReflectionUtilTest {

    private ReflectionUtil testee = new ReflectionUtil();

    class Foo {
        double add (double a, double b) {
            return a + b;
        }
    }

    @Test
    public void testCall() {
        Object o = new Foo();
        Object result = testee.call(o,"add", 1d,  2d);
        assertEquals(result, 3d);
    }
}
