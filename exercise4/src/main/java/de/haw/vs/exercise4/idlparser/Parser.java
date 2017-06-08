package de.haw.vs.exercise4.idlparser;

import com.sun.xml.internal.bind.v2.model.core.ID;
import de.haw.vs.exercise4.idlparser.IDLCompiler.MethodData;
import de.haw.vs.exercise4.idlparser.IDLCompiler.SupportedDataTypes;


import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Parser main class.
 * 
 * @author  (c) H. Schulz, 2016  
 * This programme is provided 'As-is', without any guarantee of any kind, implied or otherwise and is wholly unsupported.
 * You may use and modify it as long as you state the above copyright.
 *
 */
public class Parser {
	public static final String BEGIN = "{";
	public static final String BEGIN_REGEX = "\\{";
	public static final String END = "};";
	public static final String MODULE = "module";
	public static final String CLASS = "class";
	public static final String PARENTHESIS_OPEN = "\\(";
	public static final String PARENTHESIS_CLOSE = "\\)";
	public static final String PARENTHESES = "[(|)]";
	public static final String PARAM_SEPARATOR = ",";
	
	/**
	 * File reader counting lines.
	 * @author   (c) H. Schulz, 2016    This programme is provided 'As-is', without any guarantee of any kind, implied or otherwise and is wholly unsupported.  You may use and modify it as long as you state the above copyright.
	 */
	private static class IDLfileReader extends BufferedReader {
		private int lineNo;
		
		public IDLfileReader(Reader in) {
			super(in);
			lineNo = 0;
		}
		
		public String readLine() throws IOException {
			lineNo++;
			return super.readLine();
		}
		
		public int getLineNo() {
			return lineNo;
		}
	}
	
	/**
	 * For printing compilation errors
	 */
	private static void printError(int lineNo, String text) {
		System.err.println("Line " + lineNo + ": " + text);
	}
	
	/**
	 * Parse IDL Module in given file.
	 */
	private static IDLmodule parseModule(IDLfileReader in) throws IOException {
		IDLclass newClass;

		String line = in.readLine();		
		String tokens[] = (line.split(BEGIN_REGEX)[0]).trim().split(" ");
		
		if (tokens != null && tokens.length>1 && tokens[0].equals(MODULE) && tokens[1] !=null && tokens[1].length()>0) {
			IDLmodule currentModule = new IDLmodule(tokens[1]);
			do {
				// parse containing classes
				newClass = parseClass(in, currentModule.getModuleName());
				if (newClass!=null) currentModule.addClass(newClass);
				
				// try to read next module
				tokens = (line.split(BEGIN_REGEX)[0]).trim().split(" ");
			} while (newClass!=null);
						
			return currentModule;
			
			
		} else {
			printError(in.getLineNo(), "Error parsing module. '" + line + "'");
			return null;
		}
	}
	
	/**
	 * Parse (next) class in a file/module.
	 * 
	 * @param in file reader
	 * @param currentModuleName name of the module currently being parsed.
	 * @return the class parsed or null if there is no class left in the file
	 * @throws IOException
	 */
	private static IDLclass parseClass(IDLfileReader in, String currentModuleName) throws IOException {
		ArrayList<MethodData> methodList = new ArrayList<MethodData>();
		
		String line = in.readLine();
		if (line != null) {
			String tokens[] = (line.split(BEGIN_REGEX)[0]).trim().split(" ");
			if (tokens != null && tokens.length > 1 && tokens[0].equals(CLASS)
					&& tokens[1] != null && tokens[1].length() > 0) {
				// name of this class
				String className = tokens[1];

				// read methods
				line = in.readLine();
				while (line != null && !line.contains(END)) {
					String[] tokens2 = line.trim().split(PARENTHESES);

					String[] tokens3 = tokens2[0].split(" ");
					String rTypeString = tokens3[0]; // return value
					String methodName = tokens3[1]; // method name

					SupportedDataTypes paramTypes[] = parseParams(in.getLineNo(),
							tokens2[1]);

					// into data container
					methodList
							.add(new MethodData(methodName, IDLCompiler.getSupportedTypeForKeyword(rTypeString), paramTypes));
					line = in.readLine();
				}

				// read class end
				if (line == null || !line.contains(END)) {
					printError(in.getLineNo(), "Error parsing class "
							+ className + ": no end mark '" + line + "'");
				}

				// method data -> array
				MethodData methodArray[] = new MethodData[methodList.size()];

				//return IDL class
				return new IDLclass(className, currentModuleName,
						methodList.toArray(methodArray));
			} else {
				if (line.contains(END)) {
					return null;
				} else {
					printError(in.getLineNo(), "Error parsing class.'" + line
							+ "'");
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
	private static SupportedDataTypes[] parseParams(int lineNo, String paramList) {
		if (paramList != null && paramList.length() > 0) {
			String[] paramEntries = paramList.trim().split(PARAM_SEPARATOR);
			
			// param data container
			SupportedDataTypes paramTypes[] = new SupportedDataTypes[paramEntries.length];
						
			for (int i=0; i<paramEntries.length; i++) {
				String[] typeAndParamName = paramEntries[i].trim().split(" ");
				
				// 0: type, 1: name
				paramTypes[i] = IDLCompiler.getSupportedTypeForKeyword(typeAndParamName[0]);
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

	public static IDLmodule parse(String file) throws IOException {
		IDLfileReader in = new IDLfileReader(new FileReader(file));
		return parseModule(in);
	}

	public static void main(String[] args) {

		if(args.length == 2) {
			String idlFileName;
			String outputFileName;
			idlFileName = args[0];
			outputFileName = args[1];

			try {

				IDLmodule module = parse(idlFileName);  // Parse IDL file
				System.out.println("Parsed idl input file successfully with following shema:");
				printModule(module);
				System.out.println("Generating java stub, writing to: " + outputFileName);
				IDLCodeGenerator codeGenerator = new IDLCodeGenerator();
				List<String> result = codeGenerator.generateCodeLines(module, new IDLToJavaTranslator());
				codeGenerator.writeToOutputFile(result, outputFileName);

			} catch (IOException e) {
				e.printStackTrace();
			}
		} else {
			System.out.println("Usage: java -jar Parser.jar <input idl filename> <output java filename>");
		}

	}

	/**
	 * testing output & example on how to access class and method data of an IDL module.
	 * 
	 * @param module
	 */
	private static void printModule(IDLmodule module) {
		System.out.println();
		System.out.println("module: "+module.getModuleName());
		
		// classes
		IDLclass[] classes = module.getClasses();
		for (int i=0; i<classes.length; i++) {
			System.out.println(" class: " + classes[i].getClassName());
			
			// methods
			MethodData[] methods = classes[i].getMethods();
			for (int k=0; k<methods.length; k++) {
				System.out.print("  method: " + IDLCompiler.getSupportedIDLDataTypeName(methods[k].getReturnType()) 
						+ " " + methods[k].getName() + " ");
				
				// parameters
				SupportedDataTypes[] paramTypes = methods[k].getParamTypes();
				for (int m=0; m<paramTypes.length; m++) {
					System.out.print(IDLCompiler.getSupportedIDLDataTypeName(paramTypes[m]) + " ");
				}
				System.out.println();
			}
		}
	}

}
