/**
 *
 */
package com.github.roycetech.rule_engine;

import java.util.List;

/**
 * @author royce
 *
 */
public class Token {
    private final Object value;
    private final int subscript;

    public Token(Object value, int subscript) {
	this.value = value;
	this.subscript = subscript;
    }

    public boolean isNegative() {
	return subscript < 0;
    }

    /**
     *
     * @param internal either '*true' or '*false'
     * @return
     */
    public boolean equalsInternal(String internal) {
	return isNegative() && this.value.equals(internal);
    }

    /**
     * Checks to see if this token based on it's value and subscript is to be
     * considered given the scenario list.
     *
     * @param scenario List of scenario tokens.
     *
     * @return true if this token is non-index (-1 subscript) and the scenario
     *         list contained this token's value or, this token is subscripted
     *         and the token value at the token index in the scenario matches.
     */
    public boolean accepts(List<Object> scenario) {
	if (isNegative()) {
	    return scenario.contains(this.value);
	}
	return scenario.get(this.subscript).equals(this.value);
    }

    @Override
    public String toString() {
	return String.valueOf(this.value) + '[' + this.subscript + ']';
    }
}
