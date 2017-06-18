package de.haw.vs.enchiridion;

import de.haw.vs.nameservice.NameServiceProtocol;
import de.haw.vs.nameservice.ObjectReference;
import org.junit.Test;

import java.io.IOException;
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
        assertEquals(new ObjectReference("test", "localhost", 42), NameService.getInstance().resolve("test"));
        threadPool.shutdown();
    }

    private byte[] rebindMessage() {
        try {
            return NameServiceProtocol.buildRebindMessage(new ObjectReference("test", "localhost", 42), "test");
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }
}
