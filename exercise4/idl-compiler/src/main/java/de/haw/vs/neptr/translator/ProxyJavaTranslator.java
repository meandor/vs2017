package de.haw.vs.neptr.translator;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.MethodData;
import de.haw.vs.neptr.idlmodel.SupportedDataTypes;

import static de.haw.vs.neptr.translator.TranslatorUtil.getSupportedJavaDataTypeName;

public class ProxyJavaTranslator implements ITranslator{

    private final String JAVA_INT = "int";
    private final String JAVA_DOUBLE = "double";
    private final String JAVA_STRING = "String";

    @Override
    public String declareModule(IDLClass idlClass) {
        return "package " + idlClass.getModuleName() + ";" + "\n"
                + "\n" +
                "import mware_lib.ObjectBroker;" + "\n"
                + "import mware_lib.ObjectReference;";
    }

    @Override
    public String openClassDeclaration(IDLClass idlClass) {
        return "public class _" +  idlClass.getClassName() + "Proxy extends _" +  idlClass.getClassName() + "ImplBase {" + "\n"
                + "\tprivate ICommunicationModule communication;\n"
                + "\tprivate ObjectReference objectReference;";
    }

    @Override
    public String closeClassDeclaration() {
        return "}";
    }

    @Override
    public String declareConstructingMethod(IDLClass idlClass) {
        return "\tpublic " + "_" +  idlClass.getClassName() + "Proxy(Object rawReference) {\n" +
                "\t\tObjectReference objectReference = (ObjectReference) rawReference; \n" +
                "\t\tthis.communication = new Communication();\n" +
                "\t\tthis.objectReference = objectReference;" + "\n"
                + "\t}";
    }

    @Override
    public String declareMethod(MethodData methodData) {
        StringBuilder builder = new StringBuilder();
        builder.append("\tpublic ");
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
        builder.append(") throws Exception {\n");

        if(methodData.getReturnType() != null){
            builder.append("\t\tObject returnValue = ");
        }

        builder.append("this.communication.invoke(this.objectReference, ");
        builder.append(methodData.getName());

        c = 'a';
        for (SupportedDataTypes ignored : methodData.getParamTypes()) {
            builder.append(", ");
            builder.append(c++);
        }

        builder.append(");");

        if(methodData.getReturnType() != null){
            builder.append("\t\n");
            builder.append("\t\tif(returnValue instanceof Exception)throw ((Exception)returnValue);\n");
            builder.append("\t\treturn (");
            builder.append(getSupportedJavaDataTypeName(methodData.getReturnType()) );
            builder.append(")returnValue;");
        }
        builder.append("\n\t}");
        return builder.toString();
    }

    @Override
    public String fileName(IDLClass idlClass) {
        return "_" + idlClass.getClassName() + "Proxy.java";
    }

}
