package genlearn;

import java.util.HashMap;

//import clojure.lang.ISeq;

public interface ISearchState {
//	ISeq primitives();
//	ISeq applicableTypes();
	ITypeNode typeNodeInstance(IType type);
	HashMap exprCache();
}
