package math_ops;

public class Calculator extends _CalculatorImplBase {
    @Override
    public double add(double a, double b) throws Exception {
        return a + b;
    }

    @Override
    public String getStr(double a) throws Exception {
        return "" + a;
    }
}
