package de.haw.vs.exercise4.client.math_ops;

import de.haw.vs.exercise4.server.math_ops._CalculatorImplBase;

public class Calculator extends _CalculatorImplBase {

    @Override
    public double add(double a, double b) throws Exception {
        return a+b;
    }

    @Override
    public String getStr(double a) throws Exception {
        throw new RuntimeException(Double.toString(a));
    }

}