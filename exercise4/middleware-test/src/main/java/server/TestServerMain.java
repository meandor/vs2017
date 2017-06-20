package server;

import math_ops.Calculator;
import mware_lib.NameService;
import mware_lib.ObjectBroker;

public class TestServerMain {
    public static void main(String[] args) {
        ObjectBroker objectBroker = ObjectBroker.init("localhost", 8080, false);
        NameService nameService = objectBroker.getNameService();
        Calculator calculator = new Calculator();
        nameService.rebind(calculator, "zumsel");
    }
}
