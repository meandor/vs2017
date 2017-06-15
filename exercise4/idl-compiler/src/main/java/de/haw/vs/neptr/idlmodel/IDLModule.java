package de.haw.vs.neptr.idlmodel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * This programme is provided 'As-is', without any guarantee of any kind, implied or otherwise and
 * is wholly unsupported. You may use and modify it as long as you state the above copyright.
 *
 * @author (c) H. Schulz, 2016
 */
public class IDLModule {

    // this module's name
    private String moduleName;

    // container of classes defined in this module
    private List<IDLClass> classes;

    public IDLModule(String name) {
        this.moduleName = name;
        classes = new ArrayList<>();
    }

    public void addClass(IDLClass newClass) {
        classes.add(newClass);
    }

    public String getModuleName() {
        return moduleName;
    }

    public IDLClass[] getClasses() {
        IDLClass classes[] = new IDLClass[this.classes.size()];
        this.classes.toArray(classes);
        return classes;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        IDLModule idlModule = (IDLModule) o;

        if (!getModuleName().equals(idlModule.getModuleName())) return false;
        return Arrays.equals(getClasses(), idlModule.getClasses());
    }

    @Override
    public int hashCode() {
        int result = getModuleName().hashCode();
        result = 31 * result + Arrays.hashCode(getClasses());
        return result;
    }
}
