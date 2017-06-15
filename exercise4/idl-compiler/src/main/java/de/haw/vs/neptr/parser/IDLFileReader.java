package de.haw.vs.neptr.parser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;

/**
 * File reader counting lines.
 * <p>
 * This programme is provided 'As-is', without any guarantee of any kind, implied or
 * otherwise and is wholly unsupported.  You may use and modify it as long as you state the above copyright.
 *
 * @author (c) H. Schulz, 2016
 */
public class IDLFileReader extends BufferedReader {

    private int lineNo;

    public IDLFileReader(Reader in) {
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
