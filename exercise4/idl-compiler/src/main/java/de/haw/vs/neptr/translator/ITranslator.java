package de.haw.vs.neptr.translator;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.MethodData;

/**
 * Translator Interface for translating an IDLClass
 */
public interface ITranslator {

    /**
     * Returns translated module declaration of class
     *
     * @param idlClass IDLClass to be translated
     * @return translated module declaration of class
     */
    public String declareModule(IDLClass idlClass);

    /**
     * Returns translated opening class declaration of class
     *
     * @param idlClass IDLClass to be translated
     * @return translated opening class declaration of class
     */
    public String openClassDeclaration(IDLClass idlClass);

    /**
     * Returns closing class declaration
     *
     * @return closing class declaration
     */
    public String closeClassDeclaration();

    /**
     * Returns translated narrowCastMethod declaration
     *
     * @param idlClass IDLClass to be translated
     * @return translated narrowCastMethod declaration
     */
    public String declareConstructingMethod(IDLClass idlClass);

    /**
     * Returns translated methods of class
     *
     * @param methodData MethodData to be translated
     * @return translated methods of class
     */
    public String declareMethod(MethodData methodData);

    /**
     * Returns translated filename of class
     *
     * @param idlClass IDLClass to be translated
     * @return translated filename of class
     */
    public String fileName(IDLClass idlClass);
}
