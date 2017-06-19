package mware_lib.communication;

import java.io.Serializable;

public class RemoteCall implements Serializable{

    private static final long serialVersionUID = 8233528997584469393L;

    private String methodName;
    private Object[] args;
    private String alias;

    public RemoteCall(String alias, String methodName, Object... args) {
        this.alias = alias;
        this.methodName = methodName;
        this.args = args;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public Object[] getArgs() {
        return args;
    }

    public void setArgs(Object[] args) {
        this.args = args;
    }

    public String getAlias() {
        return alias;
    }

    public void setAlias(String alias) {
        this.alias = alias;
    }
}
