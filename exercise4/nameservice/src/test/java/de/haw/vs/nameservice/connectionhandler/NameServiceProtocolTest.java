package de.haw.vs.nameservice.connectionhandler;

import org.junit.Test;

import static org.junit.Assert.*;

public class NameServiceProtocolTest {

    @Test
    public void extractAlias() throws Exception {
        byte[] message = new byte[]{
                0x1, 0x61, 0x20, 0x20, 0x61, 0x20, 0x20, 0x61, 0x20, 0x20, 0x61, 0x20, 0x20, 0x1
        };

        assertEquals("a  a  a  a", NameServiceProtocol.extractAlias(message));
    }
}