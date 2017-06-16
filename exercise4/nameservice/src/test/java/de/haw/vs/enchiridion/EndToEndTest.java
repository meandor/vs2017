package de.haw.vs.enchiridion;

import org.junit.Test;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.junit.Assert.assertEquals;

public class EndToEndTest {

    private final ExecutorService threadPool = Executors.newFixedThreadPool(10);

    @Test
    public void testStartingNameService() throws Exception {
        Thread mainRunner = new Thread(() -> NameServiceStarter.main(new String[]{"27015"}));
        mainRunner.start();
        Thread.sleep(300);

        assertEquals(Thread.State.RUNNABLE, mainRunner.getState());
        mainRunner.interrupt();
    }

    @Test
    public void testRebindAndResolveScenario() throws Exception {
        threadPool.submit(() -> NameServiceStarter.main(new String[]{"27017"}));
        Thread.sleep(1000);
        Runnable rebindClient = new RebindTestClient(27017, "localhost", rebindMessage());
        threadPool.submit(rebindClient);
        Thread.sleep(1000);
        assertEquals("ase", NameService.getInstance().resolve("test"));
        threadPool.shutdown();
    }

    private byte[] rebindMessage() {
        return new byte[]{
                0x0,
                0x74, 0x65, 0x73, 0x74,
                0x0, 0x0, 0x0, 0x0,
                0x0, 0x0, 0x0, 0x0,
                (byte) 0xAC, (byte) 0xED, 0x0, 0x5,
                0x74, 0x0, 0x3, 0x61, 0x73, 0x65};

    }
}
