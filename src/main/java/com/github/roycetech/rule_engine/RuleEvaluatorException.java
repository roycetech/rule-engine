package com.github.roycetech.rule_engine;

/** Specialized exception for parse error. */
public class RuleEvaluatorException extends RuntimeException {

	private static final long serialVersionUID = -9018349758762438623L;

	/** @param string exception message. */
	public RuleEvaluatorException(final String string) {
		super(string);
	}
}