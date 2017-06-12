package de.haw.vs.nameservice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class NameServiceStarter {

    private final Logger logger = LoggerFactory.getLogger(NameServiceStarter.class);

    public static void main(String[] args) {
        NameServiceStarter starter = new NameServiceStarter();

        if (args.length == 1) {
            int port = Integer.parseInt(args[0]);
            starter.logger.info("Starting NameService at " + String.valueOf(port));
        } else {
            System.out.println(starter.helpText());
        }
    }

    String helpText() {
        return "Execute: nameservice [PORT]\n" +
                "Will start the nameservice at the given port.\n";
    }
}
