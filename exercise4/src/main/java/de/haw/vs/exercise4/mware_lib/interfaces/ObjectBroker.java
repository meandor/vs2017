package de.haw.vs.exercise4.mware_lib.interfaces;

public interface ObjectBroker {
    Object call(String reference, String methodName, Object... args);
}
