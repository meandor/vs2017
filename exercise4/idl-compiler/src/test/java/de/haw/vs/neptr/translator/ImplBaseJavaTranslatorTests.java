package de.haw.vs.neptr.translator;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.MethodData;
import de.haw.vs.neptr.idlmodel.SupportedDataTypes;
import org.junit.Test;

import static de.haw.vs.neptr.idlmodel.SupportedDataTypes.DOUBLE;
import static org.junit.Assert.assertEquals;

public class ImplBaseJavaTranslatorTests {

    private ImplBaseJavaTranslator testee = new ImplBaseJavaTranslator();

    @Test
    public void testModuleDeclaration() {
        String moduleDeclaration = testee.declareModule(new IDLClass(null, "math_ops", null));
        assertEquals("package math_ops;", moduleDeclaration);
    }

    @Test
    public void testImplementNarrowCast() {
        String narrowCastMethod = testee.declareConstructingMethod(new IDLClass("Calculator", "math_ops", null));
        String expected =
                "\tpublic static _CalculatorImplBase narrowCast(Object rawObjectRef) {\n" +
                        "\t\treturn new _CalculatorProxy(rawObjectRef);\n" +
                        "\t}";
        assertEquals(expected, narrowCastMethod);
    }

    @Test
    public void testOpenClassDeclaration() {
        String classDeclaration = testee.openClassDeclaration(new IDLClass("Calculator", "math_ops", null));
        String expected = "public abstract class _CalculatorImplBase {";
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
        String classDeclaration = testee.declareMethod(methodData);
        String expected = "\tpublic abstract double add(double a, double b) throws Exception;";
        assertEquals(expected, classDeclaration);
    }
}
