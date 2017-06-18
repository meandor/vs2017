package mware_lib.nameservice;

import java.io.Serializable;

public class ObjectReference implements Serializable {

    private String alias;
    private String hostname;
    private int port;
    private static final long serialVersionUID = 8233528997584469393L;


    public ObjectReference(String alias, String hostname, int port) {
        this.alias = alias;
        this.hostname = hostname;
        this.port = port;
    }

    public String getAlias() {
        return alias;
    }

    public String getHostname() {
        return hostname;
    }

    public int getPort() {
        return port;
    }
}
