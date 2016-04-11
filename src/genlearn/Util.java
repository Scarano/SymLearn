package genlearn;

import org.apache.commons.math3.util.FastMath;

public class Util {

	static final double LOGSUM_THRESHOLD = 745.0;
	public static double logSum(double lx, double ly) {
	    if (lx == Double.NEGATIVE_INFINITY) return ly;
	    if (ly == Double.NEGATIVE_INFINITY) return lx;
	    double d = lx - ly;
	    if (d >= 0) {
	        if (d > LOGSUM_THRESHOLD) return lx;
	        else return lx + Math.log1p(FastMath.exp(-d));
	    }
	    else {
	        if (d < -LOGSUM_THRESHOLD) return ly;
	        else return ly + Math.log1p(FastMath.exp(d));
	    }
	}

	public static void main(String[] args) {
	}

}
