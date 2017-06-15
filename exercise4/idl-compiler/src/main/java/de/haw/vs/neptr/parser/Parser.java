package de.haw.vs.neptr.parser;

import de.haw.vs.neptr.idlmodel.IDLClass;
import de.haw.vs.neptr.idlmodel.IDLModule;
import de.haw.vs.neptr.idlmodel.MethodData;
import de.haw.vs.neptr.idlmodel.SupportedDataTypes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Parser {
    private final String BEGIN_REGEX = "\\{";
    private final String END = "};";
    private final String MODULE = "module";
    private final String CLASS = "class";
    private final String PARENTHESES = "[(|)]";
    private final String PARAM_SEPARATOR = ",";
    private final String IDL_KEYWORD_INT = "int";
    private final String IDL_KEYWORD_DOUBLE = "double";
    private final String IDL_KEYWORD_STRING = "string";
    private final String JAVA_INT = "int";
    private final String JAVA_DOUBLE = "double";
    private final String JAVA_STRING = "String";
    private final Logger logger = LoggerFactory.getLogger(Parser.class);

    /**
     * For printing compilation errors
     */
    private void printError(int lineNo, String text) {
        logger.warn("Line " + lineNo + ": " + text);
    }

    /**
     * Parses IDLFile at the given location and returns it as a module
     *
     * @param file String file location
     * @return IDLModule of parsed content of file
     * @throws IOException Throws if files were not found
     */
    public IDLModule parse(String file) throws IOException {
        IDLFileReader in = new IDLFileReader(new FileReader(file));
        return this.parseModule(in);
    }

    /**
     * Parse IDL Module in given file.
     */
    private IDLModule parseModule(IDLFileReader in) throws IOException {
        String line = in.readLine();
        String tokens[] = (line.split(BEGIN_REGEX)[0]).trim().split(" ");
        IDLClass newClass;

        if (tokens != null && tokens.length > 1 && tokens[0].equals(MODULE) && tokens[1] != null && tokens[1].length() > 0) {
            IDLModule currentModule = new IDLModule(tokens[1]);
            do {
                // parse containing classes
                newClass = parseClass(in, currentModule.getModuleName());
                if (newClass != null) currentModule.addClass(newClass);
            } while (newClass != null);

            return currentModule;
        } else {
            printError(in.getLineNo(), "Error parsing module. '" + line + "'");
            return null;
        }
    }

    /**
     * Parse (next) class in a file/module.
     *
     * @param in                file reader
     * @param currentModuleName name of the module currently being parsed.
     * @return the class parsed or null if there is no class left in the file
     * @throws IOException thrown when file reader can not read anymore
     */
    private IDLClass parseClass(IDLFileReader in, String currentModuleName) throws IOException {
        List<MethodData> methodList = new ArrayList<>();
        String line = in.readLine();

        if (line != null) {
            String tokens[] = (line.split(BEGIN_REGEX)[0]).trim().split(" ");
            if (tokens != null && tokens.length > 1 && tokens[0].equals(CLASS) && tokens[1] != null && tokens[1].length() > 0) {
                // name of this class
                String className = tokens[1];

                // read methods
                line = in.readLine();
                while (line != null && !line.contains(END)) {
                    String[] tokens2 = line.trim().split(PARENTHESES);

                    String[] tokens3 = tokens2[0].split(" ");
                    String rTypeString = tokens3[0]; // return value
                    String methodName = tokens3[1]; // method name

                    SupportedDataTypes paramTypes[] = parseParams(in.getLineNo(), tokens2[1]);

                    // into data container
                    methodList.add(new MethodData(methodName, getSupportedTypeForKeyword(rTypeString), paramTypes));
                    line = in.readLine();
                }

                // read class end
                if (line == null || !line.contains(END)) {
                    printError(in.getLineNo(), "Error parsing class " + className + ": no end mark '" + line + "'");
                }

                // method data -> array
                MethodData methodArray[] = new MethodData[methodList.size()];

                //return IDL class
                return new IDLClass(className, currentModuleName, methodList.toArray(methodArray));
            } else {
                if (line.contains(END)) {
                    return null;
                } else {
                    printError(in.getLineNo(), "Error parsing class.'" + line + "'");
                    return null;
                }
            }
        } else {
            printError(in.getLineNo(), "Attempt to read beyond end of file.");
            return null;
        }
    }

    /**
     * Evaluate parameter list. (No reading done here!)
     */
    private SupportedDataTypes[] parseParams(int lineNo, String paramList) {
        if (paramList != null && paramList.length() > 0) {
            String[] paramEntries = paramList.trim().split(PARAM_SEPARATOR);

            // param data container
            SupportedDataTypes paramTypes[] = new SupportedDataTypes[paramEntries.length];

            for (int i = 0; i < paramEntries.length; i++) {
                String[] typeAndParamName = paramEntries[i].trim().split(" ");

                // 0: type, 1: name
                paramTypes[i] = getSupportedTypeForKeyword(typeAndParamName[0]);
                if (paramTypes[i] == null) {
                    printError(lineNo, "Error parsing param list");
                    return null;
                }
            }
            return paramTypes;
        } else {
            return new SupportedDataTypes[0];  // empty list
        }
    }

    /**
     * Get supported data type for given keyword.
     */
    private SupportedDataTypes getSupportedTypeForKeyword(String keyword) {
        switch (keyword) {
            case IDL_KEYWORD_DOUBLE:
                return SupportedDataTypes.DOUBLE;
            case IDL_KEYWORD_INT:
                return SupportedDataTypes.INT;
            case IDL_KEYWORD_STRING:
                return SupportedDataTypes.STRING;
            default:
                return null;
        }
    }
}
