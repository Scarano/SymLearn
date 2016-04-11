package genlearn;

public class FNV1a {

    private static final long FNV_64_INIT = 0xcbf29ce484222325L;
    private static final long FNV_64_PRIME = 0x100000001b3L;

    public static long longHash(byte[] data) {
    	return longHash(FNV_64_INIT, data);
    }
    public static long longHash(long initial, byte[] data) {
    	return longHash(initial, data, 0, data.length);
    }
    public static long longHash(long initial, byte[] data, int start, int end) {
        long hash = initial;
        for (int i = 0; i < end; i++) {
            hash ^= (long) data[i] & 0xff;
            hash *= FNV_64_PRIME;
        }
        return hash;
    }
    
    public static long longHash(long data) {
    	return longHash(FNV_64_INIT, data);
    }
    public static long longHash(long initial, long data) {
    	long hash = initial;
    	for (int i = 0; i < 8; i++) {
    		hash ^= data & 0xff;
    		hash *= FNV_64_PRIME;
    		data >>>= 8;
    	}
    	return hash;
    }

    public static long longHash(int data) {
    	return longHash(FNV_64_INIT, data);
    }
    public static long longHash(long initial, int data) {
    	long hash = initial;
    	for (int i = 0; i < 4; i++) {
    		hash ^= data & 0xff;
    		hash *= FNV_64_PRIME;
    		data >>>= 8;
    	}
    	return hash;
    }

    public static long longHash(double data) {
    	return longHash(Double.doubleToLongBits(data));
    }
    public static long longHash(long initial, double data) {
    	return longHash(initial, Double.doubleToLongBits(data));
    }

    public static long longHash(String s) {
    	return longHash(FNV_64_INIT, s);
    }
    public static long longHash(long initial, String s) {
        long hash = initial;
        final int len = s.length();
        for (int i = 0; i < len; i++) {
        	char c = s.charAt(i);
        	assert c == (c & 0xff);
            hash ^= c;
            hash *= FNV_64_PRIME;
        }
        return hash;
    }
}
