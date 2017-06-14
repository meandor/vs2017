package mware_lib;

import java.io.Serializable;

public class RawObject implements Serializable {

    private static final long serialVersionUID = -3688393866021967126L;
    private String host;
    private int port;
    private String ref;

    public RawObject(String host, int port, String ref) {
        this.host = host;
        this.port = port;
        this.ref = ref;
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public String getRef() {
        return ref;
    }

    public void setRef(String ref) {
        this.ref = ref;
    }

    public String toString() {
        return "RawObject: host(" + host + ") port(" + port + ") ref(" + ref.toString() + ")";
    }

}