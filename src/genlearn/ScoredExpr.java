package genlearn;

import clojure.lang.IMapEntry;

public class ScoredExpr implements IMapEntry {

	public final IExpr expr;
	public final double score;
	
	public ScoredExpr(IExpr value, double score) {
		this.expr = value;
		this.score = score;
	}
	
	public IValue value() { return (IValue) expr; }
	public IApp app() { return (IApp) expr; }
	
	public boolean equals(Object o) {
		if (o == null || !(o instanceof ScoredExpr))
			return false;
		ScoredExpr other = (ScoredExpr) o;
		return expr.equals(other.expr) && score == other.score;
	}
	
	public int hashCode() {
		return expr.hashCode() ^ Double.hashCode(score);
	}
	
	public String toString() {
		return String.format("%s <%.3f>", expr, score);
	}

	public ScoredExpr withScore(double newScore) {
		return new ScoredExpr(expr, newScore);
	}

	
	// IMapEntry interface -- allows Clojure to treat these as pairs, e.g. for 
	// structured assignment
	
	@Override public Object getKey() {
		return expr;
	}

	@Override public Object getValue() {
		return score;
	}

	@Override public Object setValue(Object value) {
		throw new UnsupportedOperationException();
	}

	@Override public Object key() {
		return expr;
	}

	@Override public Object val() {
		return score;
	}
	
	public static void main(String[] args) {
		System.out.println("hello");
	}
}
