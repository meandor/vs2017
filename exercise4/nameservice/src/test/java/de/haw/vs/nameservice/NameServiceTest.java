package de.haw.vs.nameservice;

import org.junit.Before;
import org.junit.Test;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import static org.junit.Assert.assertEquals;

public class NameServiceTest {

    private NameService testee;

    @Before
    public void setUp() throws Exception {
        this.testee = NameService.getInstance();
    }

    @Test
    public void testSingletonInstance() throws Exception {
        assertEquals(this.testee, NameService.getInstance());
        assertEquals(this.testee, NameService.getInstance());
    }

    @Test
    public void testAddingOneObject() throws Exception {
        String expected = "test";
        this.testee.rebind(expected, "test");
        Object actual = this.testee.resolve("test");
        assertEquals(expected, actual);
    }

    @Test
    public void testRebindingOneObjectMultipleTimes() throws Exception {
        String test1 = "test";
        String test2 = "foo";
        String test3 = "bar";
        this.testee.rebind(test1, "test");
        this.testee.rebind(test2, "test");
        this.testee.rebind(test3, "test");
        Object actual = this.testee.resolve("test");
        assertEquals(test3, actual);
    }

    @Test
    public void testRebindingThreadsafe() throws Exception {
        String test1 = "test";
        String test2 = "foo";
        String test3 = "bar";
        Executor pool = Executors.newFixedThreadPool(3);
        pool.execute(() -> this.testee.rebind(test1, "test"));
        pool.execute(() -> this.testee.rebind(test2, "test"));
        pool.execute(() -> this.testee.rebind(test3, "test"));
        Thread.sleep(300);

        Object actual = this.testee.resolve("test");
        assertEquals(test3, actual);
        Thread.sleep(300);

        pool.execute(() -> this.testee.rebind(test1, "test"));
        pool.execute(() -> assertEquals(test1, this.testee.resolve("test")));
        pool.execute(() -> this.testee.rebind(test3, "test"));
        Thread.sleep(300);

        assertEquals(test3, this.testee.resolve("test"));
    }
}
