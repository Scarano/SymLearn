package genlearn;

public class AppEntry {

	ISearchState state;
	IValue router;
	IType lhsType, rhsType;
	ITypeNode lhs, rhs;
	
	public AppEntry(ISearchState state, IValue router, IType lhsType, IType rhsType) {
		this.state = state;
		this.router = router;
		this.lhsType = lhsType;
		this.rhsType = rhsType;
		this.lhs = null;
		this.rhs = null;
	}

	public IValue router() {
		return router;
	}
	
	public ITypeNode lhs() {
		if (lhs == null)
			lhs = state.typeNodeInstance(lhsType);
		return lhs;
	}

	public ITypeNode rhs() {
		if (rhs == null)
			rhs = state.typeNodeInstance(rhsType);
		return rhs;
	}

	public static void main(String[] args) {
	}

}
