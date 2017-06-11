package de.haw.vs.exercise04;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class IDLCodeGenerator {
    public List<String> generateCodeLines(IDLmodule idLmodule, IDLTranslator translator) {
        List<String> result = new ArrayList<>();
        result.add(translator.declareModule(idLmodule));
        for (IDLclass idLclass : idLmodule.getClasses()) {
            result.add(translator.openClassDeclaration(idLclass));
            result.add(translator.declareNarrowCastMethod(idLclass));
            for (IDLCompiler.MethodData methodData : idLclass.getMethods()) {
                result.add(translator.declareMethod(methodData));
            }
            result.add(translator.closeClassDeclaration());
        }
        return result;
    }

    public void writeToOutputFile(List<String> parsedLines, String filename) throws IOException {

        if (!filename.endsWith(".java")) {
            filename = filename + ".java";
        }

        File fout = new File(filename);
        FileOutputStream fos = new FileOutputStream(fout);

        BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(fos));

        for (String line : parsedLines) {
            bw.write(line);
            bw.newLine();
            bw.newLine();
        }

        bw.close();
    }

}
