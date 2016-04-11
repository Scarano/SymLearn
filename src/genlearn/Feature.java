package genlearn;

public class Feature {
	
	public final IValue id;
	public final IValue value;

	/* Assumes that ids are "interned", i.e., that id1.equals(id2) implies id1 == id2.
	 * Otherwise, Feature.equals() will not work.
	 */
	public Feature(IValue id, IValue value) {
		this.id = id;
		this.value = value;
	}

	@Override
	public boolean equals(Object o) {
		if (o == null || !(o instanceof Feature))
			return false;
		Feature other = (Feature) o;
		return id == other.id && value.equals(other.value);
	}
	
	@Override
	public int hashCode() {
		return id.hashCode() + 31 * value.hashCode();
	}
}
