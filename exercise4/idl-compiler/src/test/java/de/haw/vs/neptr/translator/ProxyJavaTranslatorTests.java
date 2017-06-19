package de.haw.vs.neptr.translator;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.MethodData;
import de.haw.vs.neptr.idlmodel.SupportedDataTypes;
import org.junit.Test;

import static de.haw.vs.neptr.idlmodel.SupportedDataTypes.DOUBLE;
import static de.haw.vs.neptr.idlmodel.SupportedDataTypes.INT;
import static org.junit.Assert.assertEquals;

public class ProxyJavaTranslatorTests {

    private ProxyJavaTranslator testee = new ProxyJavaTranslator();

    @Test
    public void testModuleDeclaration() {
        String moduleDeclaration = testee.declareModule(new IDLClass(null, "math_ops", null));
        assertEquals("package math_ops;\n" +
                "\n" +
                "import de.haw.vs.nameservice.ObjectReference;\n" +
                "import mware_lib.ObjectBroker;\n", moduleDeclaration);
    }

    @Test
    public void testImplementNarrowCast() {
        String constructor = testee.declareConstructingMethod(new IDLClass("Calculator", "math_ops", null));
        String expected =
                "\tpublic _CalculatorProxy(Object rawReference) {\n" +
                        "\t\tObjectReference objectReference = (ObjectReference) rawReference; \n" +
                        "\t\tthis.objectBroker = ObjectBroker.init(\"\", 0, false);\n" +
                        "\t\tthis.objectReference = objectReference;\n" +
                        "\t}";
        assertEquals(expected, constructor);
    }

    @Test
    public void testOpenClassDeclaration() {
        String classDeclaration = testee.openClassDeclaration(new IDLClass("Calculator", "math_ops", null));
        String expected = "public class _CalculatorProxy extends _CalculatorImplBase {\n" +
                "\tprivate ObjectBroker objectBroker;\n" +
                "\tprivate ObjectReference objectReference;";
        assertEquals(expected, classDeclaration);
    }

    @Test
    public void testCloseClassDeclaration() {
        String classDeclaration = testee.closeClassDeclaration();
        String expected = "}";
        assertEquals(expected, classDeclaration);
    }

    @Test
    public void testMethodDeclaration() {
        MethodData methodData = new MethodData("add", DOUBLE, new SupportedDataTypes[]{DOUBLE, DOUBLE});
        String methodDeclaration = testee.declareMethod(methodData);
        String expected = "\tpublic double add(double a, double b) throws Exception {\n" +
                "\t\tObject returnValue = this.objectBroker.remoteCall(this.objectReference, \"add\", a, b);\t\n" +
                "\t\tif(returnValue instanceof Exception)throw ((Exception)returnValue);\n" +
                "\t\treturn (double)returnValue;\n" +
                "\t}";
        assertEquals(expected, methodDeclaration);
    }

    @Test
    public void testVoidMethodDeclaration() {
        MethodData methodData = new MethodData("foo", null, new SupportedDataTypes[]{INT});
        String methodDeclaration = testee.declareMethod(methodData);
        String expected = "\tpublic void foo(int a) throws Exception {\n" +
                "\t\tthis.objectBroker.remoteCall(this.objectReference, \"foo\", a);\n" +
                "\t}";
        assertEquals(expected, methodDeclaration);
    }
}
