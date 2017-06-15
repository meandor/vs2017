package de.haw.vs.neptr;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.IDLModule;
import de.haw.vs.neptr.idlmodel.MethodData;
import de.haw.vs.neptr.idlmodel.SupportedDataTypes;
import org.junit.Before;
import org.junit.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import static de.haw.vs.neptr.idlmodel.SupportedDataTypes.DOUBLE;
import static de.haw.vs.neptr.idlmodel.SupportedDataTypes.STRING;
import static org.junit.Assert.assertEquals;

public class CompilerTests {

    private Compiler testee;
    private IDLModule module;

    @Before
    public void setUp() throws Exception {
        this.testee = new Compiler();
        this.module = new IDLModule("math_ops");
        MethodData[] methods = new MethodData[]{
                new MethodData("add", DOUBLE, new SupportedDataTypes[]{DOUBLE, DOUBLE}),
                new MethodData("getStr", STRING, new SupportedDataTypes[]{DOUBLE}),
        };
        IDLClass calculator = new IDLClass("Calculator", "math_ops", methods);
        this.module.addClass(calculator);
    }

    @Test
    public void testCodeGenerator() throws Exception {
        List<String> actual = testee.generateClassCodeLines(this.module.getClasses()[0], new TestTranslator());
        List<String> expected = new ArrayList<>();
        expected.add("module:math_ops");
        expected.add("class:Calculator");
        expected.add("narrow-cast:Calculator");
        expected.add("method:add:[DOUBLE, DOUBLE]:DOUBLE");
        expected.add("method:getStr:[DOUBLE]:STRING");
        expected.add("close-class");

        assertEquals(6, actual.size());
        assertEquals(expected, actual);
    }

    @Test
    public void testWriteToFile() throws Exception {
        final String outputPath = "./src/test/resources/";
        Files.deleteIfExists(Paths.get(outputPath + "_TestCalculator.java"));
        List<String> expected = new ArrayList<>();
        expected.add("module:math_ops");
        expected.add("class:Calculator");
        expected.add("narrow-cast:Calculator");
        expected.add("method:add:[DOUBLE, DOUBLE]:DOUBLE");
        expected.add("method:getStr:[DOUBLE]:STRING");
        expected.add("close-class");

        testee.writeModuleClasses(outputPath, this.module, new TestTranslator());
        List<String> actual = Files.readAllLines(Paths.get(outputPath + "_TestCalculator.java"), StandardCharsets.UTF_8);
        assertEquals(expected, actual);
    }
}
