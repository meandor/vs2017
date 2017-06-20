package math_ops;

import de.haw.vs.nameservice.ObjectReference;
import mware_lib.ObjectBroker;

public class _CalculatorProxy extends _CalculatorImplBase {
	private ObjectBroker objectBroker;
	private ObjectReference objectReference;
	public _CalculatorProxy(Object rawReference) {
		ObjectReference objectReference = (ObjectReference) rawReference; 
		this.objectBroker = ObjectBroker.init("", 0, false);
		this.objectReference = objectReference;
	}
	public double add(double a, double b) throws Exception {
		Object returnValue = this.objectBroker.remoteCall(this.objectReference, "add", a, b);	
		if(returnValue instanceof Exception)throw ((Exception)returnValue);
		return (double)returnValue;
	}
	public String getStr(double a) throws Exception {
		Object returnValue = this.objectBroker.remoteCall(this.objectReference, "getStr", a);	
		if(returnValue instanceof Exception)throw ((Exception)returnValue);
		return (String)returnValue;
	}
}
