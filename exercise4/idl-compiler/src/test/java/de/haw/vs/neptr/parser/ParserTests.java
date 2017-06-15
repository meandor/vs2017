package de.haw.vs.neptr.parser;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.IDLModule;
import de.haw.vs.neptr.idlmodel.MethodData;
import de.haw.vs.neptr.idlmodel.SupportedDataTypes;
import org.junit.Before;
import org.junit.Test;

import static de.haw.vs.neptr.idlmodel.SupportedDataTypes.DOUBLE;
import static de.haw.vs.neptr.idlmodel.SupportedDataTypes.STRING;
import static org.junit.Assert.assertEquals;

public class ParserTests {

    private Parser testee;

    @Before
    public void setUp() throws Exception {
        this.testee = new Parser();
    }

    @Test
    public void testParsingIDLFile() throws Exception {
        String IDLfileName = "./src/test/resources/calc.idl";
        IDLModule actual = testee.parse(IDLfileName);

        assertEquals("math_ops", actual.getModuleName());
        assertEquals(1, actual.getClasses().length);

        MethodData[] methods = new MethodData[]{
                new MethodData("add", DOUBLE, new SupportedDataTypes[]{DOUBLE, DOUBLE}),
                new MethodData("getStr", STRING, new SupportedDataTypes[]{DOUBLE}),
        };
        IDLClass expectedClass = new IDLClass("Calculator", "math_ops", methods);
        assertEquals(expectedClass, actual.getClasses()[0]);
    }
}
