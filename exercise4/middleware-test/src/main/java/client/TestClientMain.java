package client;

import math_ops.*;
import mware_lib.NameService;
import mware_lib.ObjectBroker;

public class TestClientMain {
    public static void main(String[] args) {
        ObjectBroker objectBroker = ObjectBroker.init("localhost", 8080, true);
        NameService nameService = objectBroker.getNameService();
        Object rawRef = nameService.resolve("zumsel");
        _CalculatorImplBase implBase = _CalculatorImplBase.narrowCast(rawRef);
        try {
            double result = implBase.add(1d, 2d);
            System.out.println(result);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
