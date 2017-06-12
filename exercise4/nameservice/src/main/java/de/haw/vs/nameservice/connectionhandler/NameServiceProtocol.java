package de.haw.vs.nameservice.connectionhandler;

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
        return new String(aliasBytes, StandardCharsets.UTF_8);
    }
}
