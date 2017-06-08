package de.haw.vs.exercise4.idlparser;

public class IDLToJavaTranslator implements IDLTranslator {

    @Override
    public String declareModule(IDLmodule idLmodule) {
        return "package " + idLmodule.getModuleName() + ";";
    }

    @Override
    public String openClassDeclaration(IDLclass idLclass) {
        return "public abstract class _" + idLclass.getClassName() + "ImplBase {";
    }

    @Override
    public String closeClassDeclaration() {
        return "}";
    }

    @Override
    public String declareNarrowCastMethod(IDLclass idLclass) {
        return "\tpublic static _" + idLclass.getClassName() + "ImplBase narrowCast(Object rawObjectRef) {\n" +
                "\t\treturn new _" + idLclass.getClassName() +  "Proxy(rawObjectRef);\n" +
                "\t}";
    }

    @Override
    public String declareMethod(IDLCompiler.MethodData methodData) {
        StringBuilder builder = new StringBuilder();
        builder.append("\tpublic abstract ");
        builder.append(IDLCompiler.getSupportedJavaDataTypeName(methodData.getReturnType()));
        builder.append(" ");
        builder.append(methodData.getName());
        builder.append("(");
        char c = 'a';
        for (IDLCompiler.SupportedDataTypes type : methodData.getParamTypes()) {
            builder.append(IDLCompiler.getSupportedJavaDataTypeName(type));
            builder.append(" ");
            builder.append(c++);
            builder.append(", ");
        }
        builder.delete(builder.length() - 2, builder.length());
        builder.append(") throws Exception;");
        return builder.toString();
    }

}
