package bancomat;

import de.haw.vs.nameservice.ObjectReference;
import mware_lib.ObjectBroker;

public class _AccountProxy extends _AccountImplBase {
	private ObjectBroker objectBroker;
	private ObjectReference objectReference;
	public _AccountProxy(Object rawReference) {
		ObjectReference objectReference = (ObjectReference) rawReference; 
		this.objectBroker = ObjectBroker.init("", 0, false);
		this.objectReference = objectReference;
	}
	public double deposit(double a) throws Exception {
		Object returnValue = this.objectBroker.remoteCall(this.objectReference, "deposit", a);	
		if(returnValue instanceof Exception)throw ((Exception)returnValue);
		return (double)returnValue;
	}
	public double withdraw(double a) throws Exception {
		Object returnValue = this.objectBroker.remoteCall(this.objectReference, "withdraw", a);	
		if(returnValue instanceof Exception)throw ((Exception)returnValue);
		return (double)returnValue;
	}
}
