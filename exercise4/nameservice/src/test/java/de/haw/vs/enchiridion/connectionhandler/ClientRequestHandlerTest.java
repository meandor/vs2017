package de.haw.vs.enchiridion.connectionhandler;

import de.haw.vs.enchiridion.NameService;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
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
        ObjectReference ref = new ObjectReference("test", "localhost", 1337);
        nameService.rebind(ref, "test");
    }

    @Test
    public void testHandleIncomingInvalidRequest() throws Exception {
        byte[] request = new byte[]{0x7, NameServiceProtocol.END_OF_MESSAGE_BYTE};
        InputStream in = new ByteArrayInputStream(request);
        byte[] expected = new byte[]{(byte) 0xAC, (byte) 0xED, 0x0, 0x5};

        this.testee.handleIncomingRequest(in);
        assertArrayEquals(expected, Files.readAllBytes(Paths.get(outputFile)));
    }

    @Test
    public void testHandleIncomingResolveRequest() throws Exception {
        byte[] request = new byte[]{
                0x1,
                0x74, 0x65, 0x73, 0x74,
                0x0, 0x0, 0x0, 0x0,
                0x0, 0x0, 0x0, 0x0, NameServiceProtocol.END_OF_MESSAGE_BYTE
        };
        InputStream in = new ByteArrayInputStream(request);

        byte[] expected = NameServiceProtocol.serializeObject(new ObjectReference("test", "localhost", 1337));

        this.testee.handleIncomingRequest(in);
        assertArrayEquals(expected, Files.readAllBytes(Paths.get(outputFile)));
    }

    @Test
    public void testHandleIncomingRebindRequest() throws Exception {
        byte[] request = NameServiceProtocol.buildRebindMessage(new ObjectReference("test", "localhost", 42), "test");
        InputStream in = new ByteArrayInputStream(request);

        this.testee.handleIncomingRequest(in);
        assertEquals(new ObjectReference("test", "localhost", 42), NameService.getInstance().resolve("test"));
    }
}
