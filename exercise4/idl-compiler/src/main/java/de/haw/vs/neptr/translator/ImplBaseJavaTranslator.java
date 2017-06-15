package de.haw.vs.neptr.translator;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.MethodData;
import de.haw.vs.neptr.idlmodel.SupportedDataTypes;

public class ImplBaseJavaTranslator implements ITranslator {

    private final String JAVA_INT = "int";
    private final String JAVA_DOUBLE = "double";
    private final String JAVA_STRING = "String";

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
    public String declareNarrowCastMethod(IDLClass idlClass) {
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
        char c = 'a';
        for (SupportedDataTypes type : methodData.getParamTypes()) {
            builder.append(getSupportedJavaDataTypeName(type));
            builder.append(" ");
            builder.append(c++);
            builder.append(", ");
        }
        builder.delete(builder.length() - 2, builder.length());
        builder.append(") throws Exception;");
        return builder.toString();
    }

    @Override
    public String fileName(IDLClass idlClass) {
        return "_" + idlClass.getClassName() + "ImplBase.java";
    }

    /**
     * Get string representation of data type
     */
    private String getSupportedJavaDataTypeName(SupportedDataTypes type) {
        switch (type) {
            case INT:
                return JAVA_INT;
            case DOUBLE:
                return JAVA_DOUBLE;
            case STRING:
                return JAVA_STRING;
            default:
                return null;
        }
    }
}
