package mware_lib.communication;

import java.io.Serializable;
import java.util.Arrays;

public class RemoteCall implements Serializable {

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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RemoteCall that = (RemoteCall) o;

        if (methodName != null ? !methodName.equals(that.methodName) : that.methodName != null) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        if (!Arrays.equals(args, that.args)) return false;
        return alias != null ? alias.equals(that.alias) : that.alias == null;

    }

    @Override
    public int hashCode() {
        int result = methodName != null ? methodName.hashCode() : 0;
        result = 31 * result + Arrays.hashCode(args);
        result = 31 * result + (alias != null ? alias.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "RemoteCall{" +
                "methodName='" + methodName + '\'' +
                ", args=" + Arrays.toString(args) +
                ", alias='" + alias + '\'' +
                '}';
    }
}
