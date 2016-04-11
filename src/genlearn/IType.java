package genlearn;

import clojure.lang.Keyword;

public interface IType {
	Keyword type();
	int var();
	IType member();
	IType lhs();
	IType rhs();
}
