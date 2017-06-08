package de.haw.vs.exercise4.idlparser;

import java.util.ArrayList;
import java.util.List;

public class IDLCodeGenerator {
    public List<String> generateCodeLines(IDLmodule idLmodule, IDLTranslator translator) {
        List<String> result = new ArrayList<>();
        result.add(translator.declareModule(idLmodule));
        for (IDLclass idLclass : idLmodule.getClasses()) {
            result.add(translator.openClassDeclaration(idLclass));
            result.add(translator.declareNarrowCastMethod(idLclass));
            for(IDLCompiler.MethodData methodData :idLclass.getMethods()) {
                translator.declareMethod(methodData);
            }
            result.add(translator.closeClassDeclaration());
        }
       return result;
    }
}
