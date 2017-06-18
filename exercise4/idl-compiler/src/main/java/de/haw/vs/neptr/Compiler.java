package de.haw.vs.neptr;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.IDLModule;
import de.haw.vs.neptr.idlmodel.MethodData;
import de.haw.vs.neptr.parser.Parser;
import de.haw.vs.neptr.translator.ITranslator;
import de.haw.vs.neptr.translator.ImplBaseJavaTranslator;
import de.haw.vs.neptr.translator.ProxyJavaTranslator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * Compiler main class.
 *
 * @author (c) H. Schulz, 2016
 *         This programme is provided 'As-is', without any guarantee of any kind, implied or otherwise and is wholly unsupported.
 *         You may use and modify it as long as you state the above copyright.
 */
public class Compiler {

    void writeModuleClasses(String outputFolder, IDLModule module, ITranslator translator) throws IOException {
        String classFileName;
        for (IDLClass idlClass : module.getClasses()) {
            classFileName = outputFolder + "/" + translator.fileName(idlClass);
            Files.write(Paths.get(classFileName), this.generateClassCodeLines(idlClass, translator));
        }
    }

    List<String> generateClassCodeLines(IDLClass idlClass, ITranslator translator) {
        List<String> result = new ArrayList<>();
        result.add(translator.declareModule(idlClass));
        result.add(translator.openClassDeclaration(idlClass));
        result.add(translator.declareConstructingMethod(idlClass));
        for (MethodData methodData : idlClass.getMethods()) {
            result.add(translator.declareMethod(methodData));
        }
        result.add(translator.closeClassDeclaration());
        return result;
    }

    public static void main(String[] args) {
        final Logger logger = LoggerFactory.getLogger(Compiler.class);

        if (args.length == 2) {
            String idlFileName = args[0];
            String outputFolder = args[1];
            Parser parser = new Parser();
            Compiler compiler = new Compiler();

            try {
                logger.info("Parsing idl file: " + idlFileName);
                IDLModule module = parser.parse(idlFileName);
                logger.info("Parsed idl input file successfully");

                logger.info("Generating Java STUBS, writing to: " + outputFolder);
                compiler.writeModuleClasses(outputFolder, module, new ImplBaseJavaTranslator());
                compiler.writeModuleClasses(outputFolder, module, new ProxyJavaTranslator());
                logger.info("Successfully compiled Java stubs");
            } catch (IOException e) {
                logger.error("Could not write files", e);
            }
        } else {
            System.out.println("Usage: neptr <input idl filename> <output folder>");
        }
    }
}
