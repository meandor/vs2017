package de.haw.vs.enchiridion;

import de.haw.vs.enchiridion.connectionhandler.IServer;
import de.haw.vs.enchiridion.connectionhandler.Server;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class NameServiceStarter {

    private final Logger logger = LoggerFactory.getLogger(NameServiceStarter.class);

    public static void main(String[] args) {
        NameServiceStarter starter = new NameServiceStarter();

        if (args.length == 1) {
            int port = Integer.parseInt(args[0]);
            starter.logger.info("Starting NameService at " + String.valueOf(port));
            starter.startNameServiceServer(port);
        } else {
            System.out.println(starter.helpText());
        }
    }

    String helpText() {
        return "Execute: enchiridion [PORT]\n" +
                "Will start the enchiridion at the given port.\n";
    }

    void startNameServiceServer(int port) {
        IServer server = new Server();
        server.initServer(port);
        server.run();
    }
}
