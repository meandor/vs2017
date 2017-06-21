package bancomat;

import de.haw.vs.nameservice.ObjectReference;
import mware_lib.ObjectBroker;

public class _BankmanagerProxy extends _BankmanagerImplBase {
	private ObjectBroker objectBroker;
	private ObjectReference objectReference;
	public _BankmanagerProxy(Object rawReference) {
		ObjectReference objectReference = (ObjectReference) rawReference; 
		this.objectBroker = ObjectBroker.init("", 0, false);
		this.objectReference = objectReference;
	}
	public String getAccountID(int a) throws Exception {
		Object returnValue = this.objectBroker.remoteCall(this.objectReference, "getAccountID", a);	
		if(returnValue instanceof Exception)throw ((Exception)returnValue);
		return (String)returnValue;
	}
}
