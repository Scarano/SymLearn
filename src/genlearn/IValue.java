package genlearn;

import clojure.lang.Keyword;

public interface IValue extends IExpr {
	Keyword id();
	IExpr source();
	Object denotation();
	
	IValue apply(IValue arg, boolean populateType, IExpr expr);
	IValue apply(IValue arg);
}

