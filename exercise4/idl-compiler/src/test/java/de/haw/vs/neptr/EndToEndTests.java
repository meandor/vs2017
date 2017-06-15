package de.haw.vs.neptr;

import org.junit.Before;
import org.junit.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class EndToEndTests {

    private final Path implBaseFile = Paths.get("./src/test/resources/_CalculatorImplBase.java");

    @Before
    public void setUp() throws Exception {
        Files.deleteIfExists(implBaseFile);
    }

    @Test
    public void testWholeSystem() throws Exception {
        Compiler.main(new String[]{"./src/test/resources/calc.idl", "./src/test/resources/"});

        assertEquals(true, Files.exists(implBaseFile));

        List<String> expectedContent = new ArrayList<>();
        expectedContent.add("package math_ops;");
        expectedContent.add("public abstract class _CalculatorImplBase {");
        expectedContent.add("\tpublic static _CalculatorImplBase narrowCast(Object rawObjectRef) {");
        expectedContent.add("\t\treturn new _CalculatorProxy(rawObjectRef);");
        expectedContent.add("\t}");
        expectedContent.add("\tpublic abstract double add(double a, double b) throws Exception;");
        expectedContent.add("\tpublic abstract String getStr(double a) throws Exception;");
        expectedContent.add("}");

        assertEquals(expectedContent, Files.readAllLines(implBaseFile));
    }
}
