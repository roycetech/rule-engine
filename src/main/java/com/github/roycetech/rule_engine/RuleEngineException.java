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

    /** @param string exception message. */
    public RuleEngineException(final String string) {
	super(string);
    }

    /**
     * @param e the exception to wrap.
     */
    public RuleEngineException(final Exception e) {
	super(e);
    }
}
