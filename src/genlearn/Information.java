package genlearn;

import java.util.Arrays;
import java.util.Collection;


public class Information {
	
	static final double log2 = Math.log(2.0);
	
	public static class EventCount {
		// Technically, features should be a set. However, for performance reasons, the caller
		// may pass in a List, if they ensure that the elements are unique and always appear
		// in the same order.
		public final Collection<Object> features;
		public final Object label;
		public double count;
		
		public EventCount(Collection<Object> features, Object label, double count) {
			this.features = features;
			this.label = label;
			this.count = count;
		}
	}
	
	public static interface EventSet extends Collection<EventCount> {}
	
//	public static class EventSet {
//		public final Object feature;
//		public final Object observation;
//		public final double count;
//	}

	/**
	 * Mutual information between a label and binary-valued feature.
	 * 
	 * @param labelCounts Number of observations of each label
	 * @param featureCounts For each label, the number of observations of the label
	 *   where the feature is true. For each label i, the number of observations where
	 *   the feature is false is assumed to be labelCounts[i] - featureCounts[i].
	 */
	public static double mutInfBinary(double[] labelCounts, double[] featureCounts) {
		double sum = 0.0;
		
		double totalCount = 0.0;
		for (double c : labelCounts) totalCount += c;
		
		double cf = 0.0; // total times feature = true
		for (double c : featureCounts) cf += c;
		double pf = cf / totalCount; // marginal p(feature)
		
		double pnf = 1.0 - pf; // marginal p(~feature)
		
		for (int i = 0; i < labelCounts.length; i++) {
			// component of sum for when feature is true
			double clf = featureCounts[i];
			if (clf > 0.0) {
				double plf = clf/totalCount; // joint p(label[i], feature)
				sum += plf * Math.log(plf / (labelCounts[i]/totalCount * pf));
			}
			
			// component of sum for when feature is false
			double clnf = labelCounts[i] - clf;
			if (clnf > 0.0) {
				double plnf = clnf/totalCount; // joint p(label[i], not feature)
				sum += plnf * Math.log(plnf / (labelCounts[i]/totalCount * pnf));
			}
		}
		
		return sum  / log2;
	}


	/**
	 * Initial, slightly-less-efficient "reference" implementation of mutInfBinary
	 */
	public static double mutInfBinaryRef(double[] labelCounts, double[] featureCounts) {
		double sum = 0.0;
		
		double totalCount = 0.0;
		for (double c : labelCounts) totalCount += c;
		
		double fTCount = 0.0; // total times feature = true
		for (double c : featureCounts) fTCount += c;
		double fFCount = totalCount - fTCount; // total times feature = false;
		
		for (int i = 0; i < labelCounts.length; i++) {
			// component of sum for when feature is true
			double c = featureCounts[i];
			if (c > 0.0) {
				sum += c/totalCount * Math.log(c/totalCount / 
						(labelCounts[i]/totalCount * fTCount/totalCount));
			}
			
			// component of sum for when feature is false
			c = labelCounts[i] - c;
			if (c > 0.0) {
				sum += c/totalCount * Math.log(c/totalCount /
						(labelCounts[i]/totalCount * fFCount/totalCount));
			}
		}
		
		return sum/log2;
	}

	public static void main(String[] args) {
		double[] labelCounts = 
				Arrays.stream(args[0].split(",")).mapToDouble(Double::parseDouble).toArray();
		double[] featureCounts = 
				Arrays.stream(args[1].split(",")).mapToDouble(Double::parseDouble).toArray();
		System.out.println(mutInfBinaryRef(labelCounts, featureCounts));
		System.out.println(mutInfBinary(labelCounts, featureCounts));
	}

}
