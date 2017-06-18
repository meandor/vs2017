package de.haw.vs.neptr.translator;

import de.haw.vs.neptr.Compiler;
import de.haw.vs.neptr.idlmodel.IDLModule;
import de.haw.vs.neptr.parser.Parser;
import org.junit.Test;

import java.io.IOException;

public class ProxyTranslatorTest {

    @Test
    public void simpleTest() {
        Compiler.main(new String[]{"./src/test/resources/calc.idl", "."});
    }
}
