package mware_lib.nameservice;

import mware_lib.communication.Communication;
import org.junit.Rule;
import org.junit.Test;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

public class CommunicationTests {

    private Communication testee = new Communication();

    private class TestReceiver implements Runnable{

        @Override
        public void run() {
            ServerSocket welcomeSocket;
            try {
                welcomeSocket = new ServerSocket(6789);
                Socket connectionSocket = welcomeSocket.accept();
                InputStream stream = connectionSocket.getInputStream();
                byte[] data = new byte[1024];
                int count = stream.read(data);
                ObjectReference reference = (ObjectReference) objectFromBytes(data);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private Object objectFromBytes(byte[] bytes) {
        ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
        ObjectInput in = null;
        try {
            in = new ObjectInputStream(bis);
            return in.readObject();
        } catch (ClassNotFoundException | IOException e) {
            e.printStackTrace();
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException ex) {
                // ignore close exception
            }
        }
        return null;
    }

    @Test
    public void testInvoke() {
        ObjectReference reference = new ObjectReference("juergen", "localhost", 6789);
        testee.invoke(reference, "add", 1, 2);
    }
}
