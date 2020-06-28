/**
 *
 */
package main.java.com.github.roycetech.rule_engine;

import java.util.List;

/**
 * @author royce
 *
 */
public class Token {
    private Object value;
    private int subscript;

    public boolean isNegative() {
	return subscript < -1;
    }

    /**
     *
     * @param internal either '*true' or '*false'
     * @return
     */
    public boolean equalsInternal(String internal) {
	return isNegative() && getValue().equals(internal);
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
	    return scenario.contains(getValue());
	}
	return scenario.get(getSubscript()).equals(getValue());
    }

    /**
     * @return the value
     */
    public Object getValue() {
	return value;
    }

    /**
     * @param value the value to set
     */
    public void setValue(Object value) {
	this.value = value;
    }

    /**
     * @return the subscript
     */
    public int getSubscript() {
	return subscript;
    }

    /**
     * @param subscript the subscript to set
     */
    public void setSubscript(int subscript) {
	this.subscript = subscript;
    }
}
