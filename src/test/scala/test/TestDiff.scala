package test

import br.gov.lexml.lexmldiff.LexmlDiff

object TestDiff extends App {
  println("Original:    " + args(0))
	println("Alternative: " + args(1))
	val res = LexmlDiff.diffAsText(args(0),args(1), 0.2, true)
	println("Result:      " + res)
}
