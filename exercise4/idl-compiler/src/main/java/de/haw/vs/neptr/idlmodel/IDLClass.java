package de.haw.vs.neptr.idlmodel;

import java.util.Arrays;

/**
 * This programme is provided 'As-is', without any guarantee of any kind, implied or otherwise
 * and is wholly unsupported.  You may use and modify it as long as you state the above copyright.
 *
 * @author (c) H. Schulz, 2016
 */
public class IDLClass {

    /**
     * module name where this class resides
     */
    private String moduleName;

    /**
     * this (IDL-)class's name
     */
    private String className;

    /**
     * methods of this class
     */
    private MethodData methods[];

    public IDLClass(String name, String module, MethodData methods[]) {
        this.className = name;
        this.moduleName = module;
        this.methods = methods;
    }

    public String getModuleName() {
        return moduleName;
    }

    public String getClassName() {
        return className;
    }

    public MethodData[] getMethods() {
        return methods;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        IDLClass idlClass = (IDLClass) o;

        if (!getModuleName().equals(idlClass.getModuleName())) return false;
        if (!getClassName().equals(idlClass.getClassName())) return false;
        return Arrays.equals(getMethods(), idlClass.getMethods());
    }

    @Override
    public int hashCode() {
        int result = getModuleName().hashCode();
        result = 31 * result + getClassName().hashCode();
        result = 31 * result + Arrays.hashCode(getMethods());
        return result;
    }
}
