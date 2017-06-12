package de.haw.vs.nameservice.connectionhandler;

import de.haw.vs.nameservice.NameService;
import org.junit.Before;
import org.junit.Test;

import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

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
    public void testHandleIncomingRequest() throws Exception {
        this.testee.handleIncomingRequest("test");
        byte[] expected = new byte[]{
                (byte) 0xAC, (byte) 0xED, 0x0, 0x5, 0x74, 0x0, 0x3, 0x61, 0x73, 0x64};
        assertArrayEquals(expected, Files.readAllBytes(Paths.get(outputFile)));
    }
}
