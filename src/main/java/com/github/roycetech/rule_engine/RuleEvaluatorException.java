package com.github.roycetech.rule_engine;

/**
 * The `RuleEvaluatorException` class is a specialized exception used to
 * represent parse errors encountered during the evaluation of logical
 * expressions within the `RuleEvaluator` class.
 *
 * <p>
 * Instances of this exception are thrown when there are issues related to
 * parsing or evaluating expressions, providing detailed error messages to aid
 * in debugging and error diagnosis.
 */
public class RuleEvaluatorException extends RuntimeException {

	private static final long serialVersionUID = -9018349758762438623L;

	/**
	 * Creates a new `RuleEvaluatorException` with the given error message.
	 *
	 * @param message The exception message providing details about the parse error.
	 */
	public RuleEvaluatorException(final String message) {
		super(message);
	}
}