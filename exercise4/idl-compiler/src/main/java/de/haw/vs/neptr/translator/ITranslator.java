package de.haw.vs.neptr.translator;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.MethodData;

public interface ITranslator {

    public String declareModule(IDLClass idlClass);

    public String openClassDeclaration(IDLClass idlClass);

    public String closeClassDeclaration();

    public String declareNarrowCastMethod(IDLClass idlClass);

    public String declareMethod(MethodData methodData);

    public String fileName(IDLClass idlClass);
}
