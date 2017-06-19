package de.haw.vs.neptr.translator;

import de.haw.vs.neptr.idlmodel.MethodData;
import de.haw.vs.neptr.idlmodel.SupportedDataTypes;

class TranslatorUtil {

    private static final String JAVA_INT = "int";
    private static final String JAVA_DOUBLE = "double";
    private static final String JAVA_STRING = "String";

    /**
     * Get string representation of data type
     */
    static String getSupportedJavaDataTypeName(SupportedDataTypes type) {
        if (type == null)
            return "void";
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

    public static void writeMethodArguments(StringBuilder builder, MethodData methodData) {
        char c = 'a';

        for (SupportedDataTypes type : methodData.getParamTypes()) {
            builder.append(getSupportedJavaDataTypeName(type));
            builder.append(" ");
            builder.append(c++);
            builder.append(", ");
        }

        builder.delete(builder.length() - 2, builder.length());
    }
}
