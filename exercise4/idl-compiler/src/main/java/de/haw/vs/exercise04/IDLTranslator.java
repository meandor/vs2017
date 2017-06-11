package de.haw.vs.exercise04;

public interface IDLTranslator {
    String declareModule(IDLmodule idLmodule);
    String openClassDeclaration(IDLclass idLclass);
    String closeClassDeclaration();
    String declareNarrowCastMethod(IDLclass idLclass);
    String declareMethod(IDLCompiler.MethodData methodData);
}
