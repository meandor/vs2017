package de.haw.vs.exercise4.idlparser;

public interface IDLTranslator {
    String declareModule(IDLmodule idLmodule);
    String openClassDeclaration(IDLclass idLclass);
    String closeClassDeclaration();
    String declareNarrowCastMethod(IDLclass idLclass);
    String declareMethod(IDLCompiler.MethodData methodData);
}
