package bancomat;
public abstract class _AccountImplBase {
	public static _AccountImplBase narrowCast(Object rawObjectRef) {
		return new _AccountProxy(rawObjectRef);
	}
	public abstract double deposit(double a) throws Exception;
	public abstract double withdraw(double a) throws Exception;
}
