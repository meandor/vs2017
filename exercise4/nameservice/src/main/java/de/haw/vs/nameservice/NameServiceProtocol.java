package de.haw.vs.nameservice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

public class NameServiceProtocol {

    public static final byte REBIND = (byte) 0x0;
    public static final byte RESOLVE = (byte) 0x1;

    public static final int MSG_TYPE_POSITION = 0;
    public static final int ALIAS_LENGTH = 12;
    public static final byte END_OF_MESSAGE_BYTE = (byte) -1;
    public static final int END_OF_MESSAGE_INT = 255;

    public static String extractAlias(byte[] message) {
        byte[] aliasBytes = Arrays.copyOfRange(message, MSG_TYPE_POSITION + 1, ALIAS_LENGTH - 1);
        Logger logger = LoggerFactory.getLogger(NameServiceProtocol.class);
        logger.info("alias-bytes:" + Arrays.toString(aliasBytes));
        String alias = new String(aliasBytes);
        logger.info("alias: " + alias);
        logger.info("cleaned alias: " + alias.replaceAll("\0", ""));
        return alias.replaceAll("\0", "");
    }

    public static ObjectReference extractObject(byte[] message) throws IOException, ClassNotFoundException {
        ObjectReference result;
        int byteOffset = MSG_TYPE_POSITION + ALIAS_LENGTH + 1;
        byte[] objectBytes = Arrays.copyOfRange(message, byteOffset, message.length);

        ByteArrayInputStream byteStream = new ByteArrayInputStream(objectBytes);
        ObjectInputStream objStream = new ObjectInputStream(byteStream);
        result = (ObjectReference) objStream.readObject();
        objStream.close();
        byteStream.close();

        return result;
    }

    public static byte[] aliasBytes(String alias) {
        int diff = alias.length() - NameServiceProtocol.ALIAS_LENGTH;
        if (diff == 0) {
            return alias.getBytes(StandardCharsets.UTF_8);
        } else if (diff > 0) {
            return alias.substring(0, NameServiceProtocol.ALIAS_LENGTH).getBytes(StandardCharsets.UTF_8);
        } else {
            byte[] result = new byte[NameServiceProtocol.ALIAS_LENGTH];
            byte[] aliasBytes = alias.getBytes(StandardCharsets.UTF_8);
            System.arraycopy(aliasBytes, 0, result, 0, aliasBytes.length);
            return result;
        }
    }

    public static byte[] serializeObject(ObjectReference object) throws IOException {
        try (ByteArrayOutputStream output = new ByteArrayOutputStream();
             ObjectOutput objectOutput = new ObjectOutputStream(output)) {
            objectOutput.writeObject(object);
            return output.toByteArray();
        }
    }

    public static byte[] buildRebindMessage(ObjectReference ref, String name) throws IOException {
        byte[] messageType = new byte[]{NameServiceProtocol.REBIND};
        byte[] alias = NameServiceProtocol.aliasBytes(name);
        byte[] rebindObject = NameServiceProtocol.serializeObject(ref);
        byte[] messageEnd = new byte[]{NameServiceProtocol.END_OF_MESSAGE_BYTE};
        byte[] serializedMessage = new byte[messageType.length + alias.length + rebindObject.length + 1];
        System.arraycopy(messageType, 0, serializedMessage, 0, messageType.length);
        System.arraycopy(alias, 0, serializedMessage, messageType.length, alias.length);
        System.arraycopy(rebindObject, 0, serializedMessage, messageType.length + alias.length, rebindObject.length);
        System.arraycopy(messageEnd, 0, serializedMessage, messageType.length + alias.length + rebindObject.length, messageEnd.length);
        return serializedMessage;
    }

    public static byte[] buildResolveMessage(String name) throws IOException {
        byte[] messageType = new byte[]{NameServiceProtocol.RESOLVE};
        byte[] alias = NameServiceProtocol.aliasBytes(name);
        byte[] messageEnd = new byte[]{NameServiceProtocol.END_OF_MESSAGE_BYTE};
        byte[] serializedMessage = new byte[messageType.length + alias.length + 1];
        System.arraycopy(messageType, 0, serializedMessage, 0, messageType.length);
        System.arraycopy(alias, 0, serializedMessage, messageType.length, alias.length);
        System.arraycopy(messageEnd, 0, serializedMessage, messageType.length + alias.length, messageEnd.length);
        return serializedMessage;
    }
}
