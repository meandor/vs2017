package de.haw.vs.exercise4.idlparser;

import org.junit.Test;

import static de.haw.vs.exercise4.idlparser.IDLCompiler.SupportedDataTypes.DOUBLE;
import static org.junit.Assert.assertEquals;

public class IDLToJavaTranslatorTests {

    private IDLToJavaTranslator javaGenerator = new IDLToJavaTranslator();

    @Test
    public void testModuleDeclaration() {
        String moduleDeclaration = javaGenerator.declareModule(new IDLmodule("math_ops"));
        assertEquals("package math_ops;", moduleDeclaration);
    }

    @Test
    public void testImplementNarrowCast() {
        String narrowCastMethod = javaGenerator.declareNarrowCastMethod(new IDLclass("Calculator", "math_ops", null));
        String expected =
                "\tpublic static _CalculatorImplBase narrowCast(Object rawObjectRef) {\n" +
                        "\t\treturn new _CalculatorProxy(rawObjectRef);\n" +
                        "\t}";
        assertEquals(expected, narrowCastMethod);
    }

    @Test
    public void testOpenClassDeclaration() {
        String classDeclaration = javaGenerator.openClassDeclaration(new IDLclass("Calculator", "math_ops", null));
        String expected = "public abstract class _CalculatorImplBase {";
        assertEquals(expected, classDeclaration);
    }

    @Test
    public void testCloseClassDeclaration() {
        String classDeclaration = javaGenerator.closeClassDeclaration();
        String expected = "}";
        assertEquals(expected, classDeclaration);
    }

    @Test
    public void testMethodDeclaration() {

        IDLCompiler.MethodData methodData = new IDLCompiler.MethodData
                (
                        "add",
                        DOUBLE,
                        new IDLCompiler.SupportedDataTypes[]{DOUBLE, DOUBLE}
                );

        String classDeclaration = javaGenerator.declareMethod(methodData);
        String expected = "\tpublic abstract double add(double a, double b) throws Exception;";
        assertEquals(expected, classDeclaration);
    }

}
