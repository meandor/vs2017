package de.haw.vs.enchiridion.connectionhandler;

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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ObjectReference that = (ObjectReference) o;

        if (getPort() != that.getPort()) return false;
        if (!getAlias().equals(that.getAlias())) return false;
        return getHostname().equals(that.getHostname());
    }

    @Override
    public int hashCode() {
        int result = getAlias().hashCode();
        result = 31 * result + getHostname().hashCode();
        result = 31 * result + getPort();
        return result;
    }

    @Override
    public String toString() {
        return "ObjectReference{" +
                "alias='" + alias + '\'' +
                ", hostname='" + hostname + '\'' +
                ", port=" + port +
                '}';
    }
}
