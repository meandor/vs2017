package de.haw.vs.nameservice.connectionhandler;

import de.haw.vs.nameservice.NameService;
import org.junit.Before;
import org.junit.Test;

import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class ClientRequestHandlerTest {

    private ClientRequestHandler testee;
    private final String outputFile = "./src/test/resources/output.txt";

    @Before
    public void setUp() throws Exception {
        Files.deleteIfExists(Paths.get(outputFile));
        this.testee = new ClientRequestHandler(new ObjectOutputStream(new FileOutputStream(outputFile)));
        NameService nameService = NameService.getInstance();
        nameService.rebind("asd", "test");
    }

    @Test
    public void testHandleIncomingInvalidRequest() throws Exception {
        byte[] request = new byte[]{0x7};
        this.testee.handleIncomingRequest(request);
        byte[] expected = new byte[]{(byte) 0xAC, (byte) 0xED, 0x0, 0x5};
        assertArrayEquals(expected, Files.readAllBytes(Paths.get(outputFile)));
    }

    @Test
    public void testHandleIncomingShutdownRequest() throws Exception {
        byte[] request = new byte[]{0x2};
        this.testee.handleIncomingRequest(request);
        assertEquals(true, this.testee.isStopping());
    }

    @Test
    public void testHandleIncomingResolveRequest() throws Exception {
        byte[] request = new byte[]{
                0x1,
                0x74, 0x65, 0x73, 0x74,
                0x0, 0x0, 0x0, 0x0,
                0x0, 0x0, 0x0, 0x0
        };
        byte[] expected = new byte[]{
                (byte) 0xAC, (byte) 0xED, 0x0, 0x5,
                0x74, 0x0, 0x3, 0x61, 0x73, 0x64
        };

        this.testee.handleIncomingRequest(request);
        assertArrayEquals(expected, Files.readAllBytes(Paths.get(outputFile)));
    }

    @Test
    public void testHandleIncomingRebindRequest() throws Exception {
        byte[] request = new byte[]{
                0x0,
                0x74, 0x65, 0x73, 0x74,
                0x0, 0x0, 0x0, 0x0,
                0x0, 0x0, 0x0, 0x0,
                (byte) 0xAC, (byte) 0xED, 0x0, 0x5,
                0x74, 0x0, 0x3, 0x61, 0x73, 0x65
        };

        this.testee.handleIncomingRequest(request);
        assertEquals("ase", NameService.getInstance().resolve("test"));
    }
}
