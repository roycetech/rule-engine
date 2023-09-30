/**
 * The `RuleEngineException` class is an application-specific exception that extends `RuntimeException`.
 * It is used to represent exceptions and errors specific to the rule engine module.
 *
 * <p>Instances of this exception can be thrown to handle rule engine-related errors in a standardized way.
 *
 * @author royce
 */
package io.github.roycetech.ruleengine;

/**
 * App-specific exception.
 *
 * @author royce
 *
 */
public class RuleEngineException extends RuntimeException {

	private static final long serialVersionUID = 4624495762307364549L;

	/**
	 * Creates a new `RuleEngineException` with the given error message.
	 *
	 * @param message The exception message providing details about the error.
	 */
	public RuleEngineException(final String message) {
		super(message);
	}

	/**
	 * Creates a new `RuleEngineException` by wrapping another exception.
	 *
	 * @param exception The exception to wrap and represent as a
	 *                  `RuleEngineException`.
	 */
	public RuleEngineException(final Exception exception) {
		super(exception);
	}
}
