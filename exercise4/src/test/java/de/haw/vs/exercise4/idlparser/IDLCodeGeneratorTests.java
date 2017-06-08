package de.haw.vs.exercise4.idlparser;

import org.junit.Test;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import static org.junit.Assert.*;

public class IDLCodeGeneratorTests {
    @Test
    public void testCodeGenerator() {
        IDLmodule module = parseTestFile();
        IDLCodeGenerator codeGenerator = new IDLCodeGenerator();
        List<String> result = codeGenerator.generateCodeLines(module, new IDLToJavaTranslator());
        assertEquals(6, result.size());
    }

    @Test
    public void testWriteToFile() throws IOException {
        IDLmodule module = parseTestFile();
        IDLCodeGenerator codeGenerator = new IDLCodeGenerator();
        List<String> result = codeGenerator.generateCodeLines(module, new IDLToJavaTranslator());
        codeGenerator.writeToOutputFile(result, "Test");
        File f = new File("Test.java");
        assertTrue(f.exists() && !f.isDirectory());
    }

    private IDLmodule parseTestFile() {
        String IDLfileName = "src/main/resources/calc.idl";
        try {
            return Parser.parse(IDLfileName);
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }
}
