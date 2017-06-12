package mware_lib;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;

public class NameServiceRequestSerializer {

    private static final byte REBIND = (byte) 0x0;
    private static final byte RESOLVE = (byte) 0x1;
    private static final byte SHUTDOWN = (byte) 0x2;

    private static final int MSG_TYPE_LENGTH = 1;
    private static final int ALIAS_LENGTH = 12;

    public static byte[] serializeRebindMessage(Object servant, String name) throws IOException {
        byte[] serializedObject = serializeObject(servant);
        byte[] serializedMessage = new byte[MSG_TYPE_LENGTH + ALIAS_LENGTH + serializedObject.length];
        serializedMessage[0] = REBIND;
        for (int i = 1; i < MSG_TYPE_LENGTH + ALIAS_LENGTH && i < name.length(); i++) {
            serializedMessage[i] = (byte) name.charAt(i);
        }
        return appendSerializedObjectToSerializedMessage(serializedObject, serializedMessage);
    }

    public static byte[] serializeResolveMessage(String name){
        byte[] serializedMessage = new byte[MSG_TYPE_LENGTH + ALIAS_LENGTH];
        serializedMessage[0] = RESOLVE;
        for (int i = 1; i < MSG_TYPE_LENGTH + ALIAS_LENGTH && i < name.length(); i++) {
            serializedMessage[i] = (byte) name.charAt(i);
        }
        return serializedMessage;
    }

    public static byte[] serializeShutdownMessage() {
        byte[] serializedMessage = new byte[MSG_TYPE_LENGTH];
        serializedMessage[0] = SHUTDOWN;
        return serializedMessage;
    }

    private static byte[] serializeObject(Object object) throws IOException {
        try (ByteArrayOutputStream output = new ByteArrayOutputStream();
             ObjectOutput objectOutput = new ObjectOutputStream(output)) {
            objectOutput.writeObject(object);
            return output.toByteArray();
        }
    }

    private static byte[] appendSerializedObjectToSerializedMessage(byte[] serializedObject, byte[] serializedMessage) {
        for (int i = 0; i < serializedObject.length; i++) {
            serializedMessage[MSG_TYPE_LENGTH + ALIAS_LENGTH + i] = serializedObject[i];
        }
        return serializedMessage;
    }
}
