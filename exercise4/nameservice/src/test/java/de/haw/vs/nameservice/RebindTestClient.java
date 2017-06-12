package de.haw.vs.nameservice;

import java.io.IOException;
import java.io.OutputStream;
import java.net.Socket;

public class RebindTestClient implements Runnable {

    private int port;
    private String hostname;
    private byte[] request;

    public RebindTestClient(int port, String hostname, byte[] request) {
        this.port = port;
        this.hostname = hostname;
        this.request = request;
    }

    @Override
    public void run() {
        Socket mySock;
        OutputStream out;

        try {
            mySock = new Socket(this.hostname, this.port);
            out = mySock.getOutputStream();
            out.write(this.request);
            out.close();
            mySock.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
