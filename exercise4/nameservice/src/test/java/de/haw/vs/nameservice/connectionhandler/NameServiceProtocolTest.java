package de.haw.vs.nameservice.connectionhandler;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class NameServiceProtocolTest {

    @Test
    public void testExtractAlias() throws Exception {
        byte[] message = new byte[]{
                0x1, 0x61, 0x20, 0x20, 0x61, 0x20, 0x20, 0x61, 0x20, 0x20, 0x61, 0x20, 0x20, 0x1
        };

        assertEquals("a  a  a  a", NameServiceProtocol.extractAlias(message));
    }

    @Test
    public void testExtractAliasWithZeroPadding() throws Exception {
        byte[] message = new byte[]{
                0x1, 0x61, 0x20, 0x20, 0x61, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0
        };

        assertEquals("a  a", NameServiceProtocol.extractAlias(message));
    }


    @Test
    public void testExtractObject() throws Exception {
        byte[] message = new byte[]{
                0x1,
                0x61, 0x20, 0x20, 0x61, 0x20, 0x20, 0x61, 0x20, 0x20, 0x61, 0x20, 0x20,
                (byte) 0xAC, (byte) 0xED, 0x0, 0x5, 0x74, 0x0, 0x3, 0x61, 0x73, 0x64
        };

        assertEquals("asd", NameServiceProtocol.extractObject(message));
    }
}