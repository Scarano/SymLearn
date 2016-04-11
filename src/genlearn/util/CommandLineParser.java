package genlearn.util;

import static java.util.Arrays.asList;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class CommandLineParser {
	
	public static class InvalidOptionException extends RuntimeException {
		private static final long serialVersionUID = 3349850718759078797L;

		public InvalidOptionException(String message) {
			super(message);
		}
	}
	
	HashMap<String, Object> options = new HashMap<String, Object>();
	ArrayList<String> args = new ArrayList<String>();

	public CommandLineParser(String optionDefs, String[] args) {
		HashMap<String, Character> optionTypes = new HashMap<String, Character>();

//		stream(optionDefs.split("\\s+")).forEach(optionDef -> {
//			if (optionDef.contains("=")) {
//				String[] parts = optionDef.split("=");
//				optionTypes.put(parts[0], parts[1].charAt(0));
//			}
//			else {
//				optionTypes.put(optionDef, 'b');
//			}
//		});

		for (String optionDef : optionDefs.split("\\s+")) {
			if (optionDef.contains("=")) {
				String[] parts = optionDef.split("=");
				optionTypes.put(parts[0], parts[1].charAt(0));
			}
			else {
				optionTypes.put(optionDef, 'b');
			}
		}

		String expectingValueFor = null;
		for (String arg : args) {
			if (expectingValueFor != null) {
				switch (optionTypes.get(expectingValueFor)) {
				case 's':
					options.put(expectingValueFor, arg); break;
				case 'i':
					options.put(expectingValueFor, Integer.parseInt(arg)); break;
				case 'f':
					options.put(expectingValueFor, Double.parseDouble(arg)); break;
				}
				expectingValueFor = null;
			}
			else if (arg.startsWith("-")) {
				if (!optionTypes.containsKey(arg))
					throw new InvalidOptionException("Unrecognized option: " + arg);
				if (optionTypes.get(arg) == 'b')
					options.put(arg, true);
				else
					expectingValueFor = arg;
			}
			else {
				this.args.add(arg);
			}
		}
	}

	public List<String> args() {
		return args;
	}
	
	public String arg(int i) {
		if (i < args.size())
			return args.get(i);
		else
			return null;
	}
	
	public String arg(int i, String defaultVal) {
		if (i < args.size())
			return args.get(i);
		else
			return defaultVal;
	}
	
	public boolean opt(String key) {
		return options.containsKey(key);
	}
	
	public String stringOpt(String key) {
		return opt(key, null);
	}
	public String opt(String key, String defaultVal) {
		return (String) options.getOrDefault(key, defaultVal);
	}
	
	public Integer intOpt(String key) {
		return (Integer) options.getOrDefault(key, null);
	}
	public int opt(String key, int defaultVal) {
		return (Integer) options.getOrDefault(key, defaultVal);
	}

	public Double floatOpt(String key) {
		return (Double) options.getOrDefault(key, null);
	}
	public double opt(String key, double defaultVal) {
		return (Double) options.getOrDefault(key, defaultVal);
	}

	public static void main(String[] args) {
		if (args.length == 0) {
			asList(
				"",
				"-opt arg1 -float 1.2 -int 123 -string foobar arg2",
				"-int 123",
				"arg1"
			).stream().forEachOrdered(argStr -> test(argStr.split(" ")));
		}
		else {
			test(args);
		}
	}
	
	public static void test(String[] args) {
		CommandLineParser clp = new CommandLineParser(
				"-opt -string=s -int=i -float=f", args);
				
		System.out.println();
		System.out.println(String.join(" ", args) + ":");
		
		System.out.println(String.join("\n", clp.args));
		
		System.out.println("-opt: " + clp.opt("-opt"));
		
		System.out.println("-string: " + clp.opt("-string", "null"));
		
		System.out.println("-int: " + clp.opt("-int", 0));
		
		System.out.println("-float: " + clp.opt("-float", 0.0));
	}
}




















