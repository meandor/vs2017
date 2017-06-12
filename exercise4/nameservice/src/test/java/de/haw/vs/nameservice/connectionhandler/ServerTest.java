package de.haw.vs.nameservice.connectionhandler;

import org.junit.Before;
import org.junit.Test;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import static org.junit.Assert.assertEquals;

public class ServerTest {

    private Server testee;

    @Before
    public void setUp() throws Exception {
        this.testee = new Server();
    }

    @Test
    public void testInitializingServer() throws Exception {
        this.testee.initServer(1337);
        assertEquals(false, this.testee.isStopping());

        this.testee.initServer(1337);
        assertEquals(true, this.testee.isStopping());
    }

    @Test
    public void testStartingAndStoppingServer() throws Exception {
        this.testee.initServer(4213);
        assertEquals(false, this.testee.isStopping());

        Executor threadPool = Executors.newFixedThreadPool(1);
        threadPool.execute(this.testee);
        Thread.sleep(300);
        this.testee.stop();
        Thread.sleep(100);
        assertEquals(true, this.testee.isStopping());
    }
}
