package de.haw.vs.enchiridion;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertNotNull;

public class NameServiceStarterTest {

    private NameServiceStarter testee;

    @Before
    public void setUp() throws Exception {
        this.testee = new NameServiceStarter();
    }

    @Test
    public void testStarterHasHelpText() {
        assertNotNull("app should have a helptext", testee.helpText());
    }
}
