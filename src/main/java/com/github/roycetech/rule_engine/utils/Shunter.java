/**
 *
 */
package com.github.roycetech.rule_engine.utils;

import java.util.Deque;

import com.github.roycetech.rule_engine.LogicHelper;
import com.github.roycetech.rule_engine.Operator;

/**
 * Applies the shunting yard algorithm.
 *
 * @author royce
 */
public class Shunter {

    /** Temporary stack that holds operators, functions and brackets. */
    private final Deque<Object> stackOperations;

    /** Stack for holding expression converted to reversed polish notation. */
    private final Deque<Object> stackRPN;

    public Shunter(final Deque<Object> stackOperations,
	    final Deque<Object> stackRPN) {
	this.stackOperations = stackOperations;
	this.stackRPN = stackRPN;
    }

    /**
     * For the Deque version, stackRPN has to be in reversed order already.
     *
     * @param token token on which to perform the shunting algorithm.
     */
    public void shuntInternal(final Object token)
    {
	final String tokenString = token.toString();
	final char tokenChar = tokenString.charAt(0);

	if (LogicHelper.isOpenBracket(tokenChar)) {
	    this.stackOperations.add(token);
	} else if (LogicHelper.isCloseBracket(tokenChar)) {
	    shuntClose();
	} else if (LogicHelper.isOperator(tokenString)) {
	    shuntOperator(token);
	} else {
	    this.stackRPN.push(token);
	}
    }

    private void shuntClose()
    {
	while (!this.stackOperations.isEmpty() && !LogicHelper.isOpenBracket(
		this.stackOperations.peekLast().toString().trim().charAt(0))) {
	    this.stackRPN.add(this.stackOperations.removeLast());
	}
	this.stackOperations.removeLast();
    }

    /**
     * @param token can be the traditional String or an array.
     */
    private void shuntOperator(final Object token)
    {
	while (!this.stackOperations.isEmpty()
		&& LogicHelper.isOperator(
			this.stackOperations.peekLast().toString().trim())
		&& Operator.fromString(token.toString())
			.getPrecedence() <= Operator.fromString(
				this.stackOperations.peekLast().toString())
				.getPrecedence()) {
	    this.stackRPN.add(this.stackOperations.removeLast());
	}
	this.stackOperations.add(token);
    }

    /**
     * @return the stackOperations. For testing purpose only.
     */
    Deque<Object> getStackOperations()
    {
	return stackOperations;
    }

    /**
     * @return the stackRPN. For testing purpose only.
     */
    Deque<Object> getStackRPN()
    {
	return stackRPN;
    }

}
