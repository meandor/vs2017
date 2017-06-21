package bancomat;
public abstract class _BankmanagerImplBase {
	public static _BankmanagerImplBase narrowCast(Object rawObjectRef) {
		return new _BankmanagerProxy(rawObjectRef);
	}
	public abstract String getAccountID(int a) throws Exception;
}
