package de.haw.vs.nameservice;

import java.io.Serializable;


public class ObjectReference implements Serializable {
    /**
     *
     */
    private static final long serialVersionUID = 1401776112847873409L;
    private final String refName;
    private final int port;
    private final String ip;

    public ObjectReference(String refName,String ip, int port) {
        this.refName = refName;
        this.port = port;
        this.ip = ip;
    }

    public String getAlias() {
        return refName;
    }

    public int getPort() {
        return port;
    }

    public String getHostname() {
        return ip;
    }

    @Override
    public String toString() {
        return refName + "/" + ip + "/" + port;
    }
}
