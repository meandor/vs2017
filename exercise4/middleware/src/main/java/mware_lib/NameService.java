package mware_lib;

/**
 * Abstract class used as an Interface for the NameService.
 */
public abstract class NameService {

    /**
     * Registers an Object (servant) at the NameService
     *
     * @param servant Object to register
     * @param name    String representation of the object
     */
    public abstract void rebind(Object servant, String name);

    /**
     * Returns the Object reference from the given servant
     *
     * @param name String of servant
     * @return general Object reference
     */
    public abstract Object resolve(String name);
}
