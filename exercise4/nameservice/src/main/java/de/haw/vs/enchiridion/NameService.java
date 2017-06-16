package de.haw.vs.enchiridion;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class NameService {

    private ConcurrentMap<String, Object> registry;
    private final Logger logger = LoggerFactory.getLogger(NameService.class);
    private static NameService instance;

    public static NameService getInstance() {
        if (instance == null) {
            instance = new NameService();
        }
        return instance;
    }

    private NameService() {
        this.registry = new ConcurrentHashMap<>();
    }

    public void rebind(Object servant, String name) {
        logger.info("Rebinding " + name + " with: " + servant.toString());
        this.registry.put(name, servant);
    }

    public Object resolve(String name) {
        Object resolved =  this.registry.getOrDefault(name, null);
        logger.info("Resolved " + name+ " to " + resolved);
        return resolved;
    }
}
