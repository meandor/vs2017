package mware_lib.communication;

import de.haw.vs.nameservice.ObjectReference;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class CommunicationModuleTest {

    private CommunicationModule testee;
    private final int port = 1337;

    private void startCommB() {
        (new Thread(new TestCommunicationModuleB(this.port))).start();
        try {
            Thread.sleep(200);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    @Before
    public void setUp() throws Exception {
        this.testee = new CommunicationModule();
        this.startCommB();
    }

    /**
     * All errors (in this case connection refused) should result in an runtime exception
     */
    @Test
    public void testInvokeRemoteDoesNotExist() {
        Object result = testee.invoke(new ObjectReference("zumsel", "localhost", 9000), "fly");
        assertEquals(result.getClass(), RuntimeException.class);
    }

    @Test
    public void testInvokeRefDoesNotExist() {
        Object result = testee.invoke(new ObjectReference("notexisting", "localhost", this.port), "fly");
        assertEquals(result.getClass(), RuntimeException.class);
    }

    @Test
    public void testInvokeRefMethodDoesNotExist() {
        Object result = testee.invoke(new ObjectReference("zumsel", "localhost", this.port), "nonexistent");
        assertEquals(result.getClass(), RuntimeException.class);
    }

    @Test
    public void testInvokeRefMethodAttributesDoNotExist() {
        Object result = testee.invoke(new ObjectReference("zumsel", "localhost", this.port), "foobar");
        assertEquals(result.getClass(), RuntimeException.class);

        this.startCommB();
        result = testee.invoke(new ObjectReference("zumsel", "localhost", this.port), "foobar", "nonsense");
        assertEquals(result.getClass(), RuntimeException.class);
    }

    @Test
    public void testInvokeGoodCase() throws Exception {
        ObjectReference ref = new ObjectReference("zumsel", "localhost", this.port);
        Object result = testee.invoke(ref, "foobar", "boo", "baz");
        assertEquals("over 9000", result);
    }
}
