package de.haw.vs.neptr.translator;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.MethodData;
import de.haw.vs.neptr.idlmodel.SupportedDataTypes;

import static de.haw.vs.neptr.translator.TranslatorUtil.getSupportedJavaDataTypeName;

public class ProxyJavaTranslator implements ITranslator{

    @Override
    public String declareModule(IDLClass idlClass) {
        return "package " + idlClass.getModuleName() + ";" + "\n"
                + "\n" +
                "import mware_lib.nameservice.ObjectReference;" + "\n"
                + "import mware_lib.ObjectBroker;" + "\n";
    }

    @Override
    public String openClassDeclaration(IDLClass idlClass) {
        return "public class _" +  idlClass.getClassName() + "Proxy extends _" +  idlClass.getClassName() + "ImplBase {" + "\n"
                + "\tprivate ObjectBroker objectBroker;\n"
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
                "\t\tthis.objectBroker = ObjectBroker.init(objectReference.getHost(), objectReference.getPort(), false);\n" +
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
        } else{
            builder.append("\t\t");
        }

        builder.append("this.objectBroker.remoteCall(this.objectReference.getAlias(), ");

        builder.append("\"");
        builder.append(methodData.getName());
        builder.append("\"");

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
