/**
 *
 */
package com.github.roycetech.rule_engine;

/**
 * App-specific exception.
 *
 * @author royce
 *
 */
public class RuleEngineException extends RuntimeException {

	private static final long serialVersionUID = 4624495762307364549L;

	/** @param string exception message. */
	public RuleEngineException(final String string) {
		super(string);
	}

	/**
	 * @param exception the exception to wrap.
	 */
	public RuleEngineException(final Exception exception) {
		super(exception);
	}
}
