package genlearn;

public interface IApp extends IExpr {
	IValue router();
	IExpr lhs();
	IExpr rhs();
}


