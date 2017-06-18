package de.haw.vs.neptr;

import org.junit.Before;
import org.junit.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class EndToEndTests {

    private final Path implBaseFile = Paths.get("./src/test/resources/_CalculatorImplBase.java");
    private final Path proxyFile = Paths.get("./src/test/resources/_Calculator2Proxy.java");

    @Before
    public void setUp() throws Exception {
        Files.deleteIfExists(implBaseFile);
    }

    @Test
    public void testWholeSystem() throws Exception {
        Compiler.main(new String[]{"./src/test/resources/empty.idl", "./src/test/resources/"});
        Compiler.main(new String[]{"./src/test/resources/calc.idl", "./src/test/resources/"});

        assertTrue(Files.exists(implBaseFile));
        assertTrue(Files.exists(proxyFile));

        List<String> expectedContent = new ArrayList<>();
        expectedContent.add("package math_ops;");
        expectedContent.add("public abstract class _CalculatorImplBase {");
        expectedContent.add("\tpublic static _CalculatorImplBase narrowCast(Object rawObjectRef) {");
        expectedContent.add("\t\treturn new _CalculatorProxy(rawObjectRef);");
        expectedContent.add("\t}");
        expectedContent.add("\tpublic abstract double add(double a, double b) throws Exception;");
        expectedContent.add("\tpublic abstract String getStr(double a) throws Exception;");
        expectedContent.add("}");


        List<String> proxyExpectedContent = new ArrayList<>();
        proxyExpectedContent.add("package math_ops2;");
        proxyExpectedContent.add("import mware_lib.nameservice.ObjectReference;");
        proxyExpectedContent.add("import mware_lib.ObjectBroker;");
        proxyExpectedContent.add("public class _Calculator2Proxy extends _Calculator2ImplBase {");
        proxyExpectedContent.add("\tprivate ObjectBroker objectBroker;");
        proxyExpectedContent.add("\tprivate ObjectReference objectReference;");
        proxyExpectedContent.add("\tpublic _Calculator2Proxy(Object rawReference) {");
        proxyExpectedContent.add("\t\tObjectReference objectReference = (ObjectReference) rawReference; ");
        proxyExpectedContent.add("\t\tthis.objectBroker = ObjectBroker.init(objectReference.getHostname(), objectReference.getPort(), false);");
        proxyExpectedContent.add("\t\tthis.objectReference = objectReference;");
        proxyExpectedContent.add("\t}");
        proxyExpectedContent.add("}");

        assertEquals(expectedContent, Files.readAllLines(implBaseFile));
        // Dont care about linebreaks
        List<String> lines = Files.readAllLines(proxyFile);
        while (lines.contains(""))
            lines.remove("");
        assertEquals(proxyExpectedContent, lines);

    }
}
