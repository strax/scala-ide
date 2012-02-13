package scala.tools.eclipse.debug.scalac;

public privileged aspect CompilerAspect {

	/** 
	 * All classes in package <code>scala.tools.nsc.symtab</code> should be only accessed by the 
	 * <code>scala.tools.nsc.interactive.PresentationCompilerThread</code> to avoid concurrency 
	 * problems such as race conditions or visibility issues.
	 * 
	 * Be aware, this pointcut *is* expensive. 
	 * */
	pointcut watchSymbols() : call(* scala.tools.nsc.symtab.*.*(..)) && within(scala.tools.eclipse..*) /*&& !within(scala.tools.nsc.interactive.CompilerControl)*/;

	before() : watchSymbols() {
		if (!isExecutingOnPresentationCompilerThread()) {
			Thread.dumpStack();
		}
	}
	
	
	private boolean isExecutingOnPresentationCompilerThread() {
		return Thread.currentThread().getName().startsWith("Scala Presentation Compiler");
	}
}
