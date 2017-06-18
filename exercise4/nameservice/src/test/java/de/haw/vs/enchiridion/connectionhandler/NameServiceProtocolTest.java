package de.haw.vs.enchiridion.connectionhandler;

import org.junit.Test;

import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

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
        ObjectReference expected = new ObjectReference("alias", "hostname", 42);
        byte[] header = new byte[]{0x1, 0x61, 0x20, 0x20, 0x61, 0x20, 0x20, 0x61, 0x20, 0x20, 0x61, 0x20, 0x20};
        byte[] object = NameServiceProtocol.serializeObject(expected);
        byte[] message = new byte[header.length + object.length];
        System.arraycopy(header, 0, message, 0, header.length);
        System.arraycopy(object, 0, message, header.length, object.length);

        assertEquals(expected, NameServiceProtocol.extractObject(message));
    }

    @Test
    public void testSerializeObject() throws Exception {
        Path objectPath = Paths.get("./src/test/resources/output.txt");
        Files.deleteIfExists(objectPath);
        ObjectReference expected = new ObjectReference("alias", "hostname", 42);
        Files.write(objectPath, NameServiceProtocol.serializeObject(expected));
        FileInputStream fileIn = new FileInputStream("./src/test/resources/output.txt");
        ObjectInputStream in = new ObjectInputStream(fileIn);
        ObjectReference actual = (ObjectReference) in.readObject();
        in.close();
        fileIn.close();

        assertEquals(expected, actual);
    }
}