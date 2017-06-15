package de.haw.vs.neptr.idlmodel;

import java.util.Arrays;

/**
 * Data container for method data.
 * <p>
 * This programme is provided 'As-is', without any guarantee of any kind, implied or otherwise and
 * is wholly unsupported. You may use and modify it as long as you state the above copyright.
 *
 * @author (c) H. Schulz, 2016
 */
public class MethodData {

    private String name;
    private SupportedDataTypes returnType;
    private SupportedDataTypes[] paramTypes;

    public MethodData(String name, SupportedDataTypes returnType, SupportedDataTypes[] paramTypes) {
        this.name = name;
        this.returnType = returnType;
        this.paramTypes = paramTypes;
    }

    public String getName() {
        return name;
    }

    public SupportedDataTypes getReturnType() {
        return returnType;
    }

    public SupportedDataTypes[] getParamTypes() {
        return paramTypes;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        MethodData that = (MethodData) o;

        if (!getName().equals(that.getName())) return false;
        if (getReturnType() != that.getReturnType()) return false;
        return Arrays.equals(getParamTypes(), that.getParamTypes());
    }

    @Override
    public int hashCode() {
        int result = getName().hashCode();
        result = 31 * result + getReturnType().hashCode();
        result = 31 * result + Arrays.hashCode(getParamTypes());
        return result;
    }
}