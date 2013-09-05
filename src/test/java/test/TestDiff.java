package test;

import br.gov.lexml.lexmldiff.LexmlDiff;

public class TestDiff {
	/*
	 * Testando interface Java --> Scala
	 * 
	 */
	public static void main(String[] args) throws Exception {
		System.out.println("Original:    " + args[0]);
		System.out.println("Alternative: " + args[1]);
		String res = LexmlDiff.diffAsText(args[0],args[1], 0.2, true);
		System.out.println("Result:      " + res);
	}
}
