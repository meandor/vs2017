package de.haw.vs.exercise04;

import java.util.Arrays;

class TestTranslator implements IDLTranslator {

    @Override
    public String declareModule(IDLmodule idLmodule) {
        return "module:" + idLmodule.getModuleName();
    }

    @Override
    public String openClassDeclaration(IDLclass idLclass) {
        return "class:" + idLclass.getClassName();
    }

    @Override
    public String closeClassDeclaration() {
        return "close-class";
    }

    @Override
    public String declareNarrowCastMethod(IDLclass idLclass) {
        return "narrow-cast:" + idLclass.getClassName();
    }

    @Override
    public String declareMethod(IDLCompiler.MethodData methodData) {
        return "method:" + methodData.getName() + ":" + Arrays.toString(methodData.getParamTypes()) + ":" + methodData.getReturnType();
    }
}
