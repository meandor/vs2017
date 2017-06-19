package de.haw.vs.neptr.translator;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.MethodData;
import de.haw.vs.neptr.idlmodel.SupportedDataTypes;

import static de.haw.vs.neptr.translator.TranslatorUtil.getSupportedJavaDataTypeName;

public class ImplBaseJavaTranslator implements ITranslator {

    @Override
    public String declareModule(IDLClass idlClass) {
        return "package " + idlClass.getModuleName() + ";";
    }

    @Override
    public String openClassDeclaration(IDLClass idlClass) {
        return "public abstract class _" + idlClass.getClassName() + "ImplBase {";
    }

    @Override
    public String closeClassDeclaration() {
        return "}";
    }

    @Override
    public String declareConstructingMethod(IDLClass idlClass) {
        return "\tpublic static _" + idlClass.getClassName() + "ImplBase narrowCast(Object rawObjectRef) {\n" +
                "\t\treturn new _" + idlClass.getClassName() + "Proxy(rawObjectRef);\n" +
                "\t}";
    }

    @Override
    public String declareMethod(MethodData methodData) {
        StringBuilder builder = new StringBuilder();
        builder.append("\tpublic abstract ");
        builder.append(getSupportedJavaDataTypeName(methodData.getReturnType()));
        builder.append(" ");
        builder.append(methodData.getName());
        builder.append("(");

        TranslatorUtil.writeMethodArguments(builder, methodData);

        builder.append(") throws Exception;");
        return builder.toString();
    }

    @Override
    public String fileName(IDLClass idlClass) {
        return "_" + idlClass.getClassName() + "ImplBase.java";
    }

}
