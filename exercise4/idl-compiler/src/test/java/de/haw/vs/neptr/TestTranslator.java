package de.haw.vs.neptr;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.MethodData;
import de.haw.vs.neptr.translator.ITranslator;

import java.util.Arrays;

class TestTranslator implements ITranslator {

    @Override
    public String declareModule(IDLClass idlClass) {
        return "module:" + idlClass.getModuleName();
    }

    @Override
    public String openClassDeclaration(IDLClass idLclass) {
        return "class:" + idLclass.getClassName();
    }

    @Override
    public String closeClassDeclaration() {
        return "close-class";
    }

    @Override
    public String declareNarrowCastMethod(IDLClass idLclass) {
        return "narrow-cast:" + idLclass.getClassName();
    }

    @Override
    public String declareMethod(MethodData methodData) {
        return "method:" + methodData.getName() + ":" + Arrays.toString(methodData.getParamTypes()) + ":" + methodData.getReturnType();
    }

    @Override
    public String fileName(IDLClass idlClass) {
        return "/_Test" + idlClass.getClassName() + ".java";
    }
}
