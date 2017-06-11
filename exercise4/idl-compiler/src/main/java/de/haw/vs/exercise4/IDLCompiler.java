package de.haw.vs.exercise4;

/**
 * IDL compiler main class.
 *
 * @author (c) H. Schulz, 2016
 *         This programme is provided 'As-is', without any guarantee of any kind, implied or otherwise and is wholly unsupported.
 *         You may use and modify it as long as you state the above copyright.
 */
public class IDLCompiler {
    public static final String NEWLINE = "\n";
    public static final String IDL_KEYWORD_INT = "int";
    public static final String IDL_KEYWORD_DOUBLE = "double";
    public static final String IDL_KEYWORD_STRING = "string";
    public static final String JAVA_INT = "int";
    public static final String JAVA_DOUBLE = "double";
    public static final String JAVA_STRING = "String";


    // Supported data types

    /**
     * @author (c) H. Schulz, 2016    This programme is provided 'As-is', without any guarantee of any kind, implied or otherwise and is wholly unsupported.  You may use and modify it as long as you state the above copyright.
     */
    public static enum SupportedDataTypes {
        INT,
        DOUBLE,
        STRING
    }

    /**
     * Get string representation of supported IDL data type
     */
    public static String getSupportedIDLDataTypeName(SupportedDataTypes type) {
        switch (type) {
            case INT:
                return IDL_KEYWORD_INT;
            case DOUBLE:
                return IDL_KEYWORD_DOUBLE;
            case STRING:
                return IDL_KEYWORD_STRING;
            default:
                return null;
        }
    }

    /**
     * Get string representation of data type
     */
    public static String getSupportedJavaDataTypeName(SupportedDataTypes type) {
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

    /**
     * Get supported data type for given keyword.
     */
    public static SupportedDataTypes getSupportedTypeForKeyword(String keyword) {
        if (keyword.equals(IDL_KEYWORD_DOUBLE)) return SupportedDataTypes.DOUBLE;
        else if (keyword.equals(IDL_KEYWORD_INT)) return SupportedDataTypes.INT;
        else if (keyword.equals(IDL_KEYWORD_STRING)) return SupportedDataTypes.STRING;
        else return null;
    }


    /**
     * Data container for method data.
     *
     * @author (c) H. Schulz, 2016    This programme is provided 'As-is', without any guarantee of any kind, implied or otherwise and is wholly unsupported.  You may use and modify it as long as you state the above copyright.
     */
    static class MethodData {
        private String name;
        private SupportedDataTypes returnType;
        private SupportedDataTypes[] paramTypes;

        public MethodData(String name, SupportedDataTypes returnType, SupportedDataTypes[] paramTypes) {
            this.name = name;
            this.returnType = returnType;
            this.paramTypes = paramTypes;
        }

        public String getName() {
            return name;
        }

        public SupportedDataTypes getReturnType() {
            return returnType;
        }

        public SupportedDataTypes[] getParamTypes() {
            return paramTypes;
        }
    }

}
