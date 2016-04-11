package genlearn.util;

import clojure.lang.Keyword;

public interface IConfig {
	public Object getValue(Keyword paramKey);
	
//	public Object getValue(Keyword paramKey, Object defaultValue);
	
	public default Object getValue(String paramName) {
		return getValue(Keyword.intern(null, paramName));
	}
//	public default Object getValue(String paramName, Object defaultValue) {
//		return getValue(Keyword.intern(null, paramName), defaultValue);
//	}
	
	public default Object getValue(String prefix, Keyword suffix) {
		return getValue(Keyword.intern(null, prefix + "." + suffix.getName()));
	}
	public default Object getValue(String prefix, String suffix) {
		return getValue(Keyword.intern(null, prefix + "." + suffix));
	}
}
