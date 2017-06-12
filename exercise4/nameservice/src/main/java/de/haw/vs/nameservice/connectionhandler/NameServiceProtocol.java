package de.haw.vs.nameservice.connectionhandler;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

public class NameServiceProtocol {

    static final byte REBIND = (byte) 0x0;
    static final byte RESOLVE = (byte) 0x1;
    static final byte SHUTDOWN = (byte) 0x2;

    static final int MSG_TYPE_POSITION = 0;
    static final int ALIAS_LENGTH = 12;

    static String extractAlias(byte[] message) {
        byte[] aliasBytes = Arrays.copyOfRange(message, MSG_TYPE_POSITION + 1, ALIAS_LENGTH - 1);
        return new String(aliasBytes, StandardCharsets.UTF_8).replaceAll("\0", "");
    }

    static Object extractObject(byte[] message) throws IOException, ClassNotFoundException {
        Object result = null;
        int byteOffset = MSG_TYPE_POSITION + ALIAS_LENGTH + 1;
        byte[] objectBytes = Arrays.copyOfRange(message, byteOffset, message.length);

        ByteArrayInputStream byteStream = new ByteArrayInputStream(objectBytes);
        ObjectInputStream objStream = new ObjectInputStream(byteStream);
        result = objStream.readObject();
        objStream.close();
        byteStream.close();

        return result;
    }

    static byte[] serializeObject(Object object) throws IOException {
        try (ByteArrayOutputStream output = new ByteArrayOutputStream();
             ObjectOutput objectOutput = new ObjectOutputStream(output)) {
            objectOutput.writeObject(object);
            return output.toByteArray();
        }
    }
}
