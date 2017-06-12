package de.haw.vs.nameservice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class NameService {

    private ConcurrentMap<String, Object> registry;
    private final Logger logger = LoggerFactory.getLogger(NameService.class);

    public NameService() {
        this.registry = new ConcurrentHashMap<>();
    }

    public void rebind(Object servant, String name) {
        logger.info("Rebinding " + name + " with: " + servant.toString());
        this.registry.put(name, servant);
    }

    public Object resolve(String name) {
        return this.registry.getOrDefault(name, null);
    }
}
