package de.haw.vs.exercise4;

import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class IDLCodeGeneratorTests {

    private IDLCodeGenerator testee;
    private IDLmodule module;

    @Before
    public void setUp() throws Exception {
        this.testee = new IDLCodeGenerator();
        this.module = parseTestFile();
    }

    private IDLmodule parseTestFile() {
        String IDLfileName = "./src/test/resources/calc.idl";
        try {
            return Compiler.parse(IDLfileName);
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Test
    public void testCodeGenerator() throws Exception {
        List<String> actual = testee.generateCodeLines(module, new TestTranslator());
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
        final String outputFile = "./src/test/resources/Test";
        Files.deleteIfExists(Paths.get(outputFile));
        List<String> expected = new ArrayList<>();
        expected.add("start");
        expected.add("stop");
        testee.writeToOutputFile(expected, outputFile);

        List<String> actual = Files.readAllLines(Paths.get(outputFile + ".java"), StandardCharsets.UTF_8);
        assertEquals(expected, actual);
    }
}