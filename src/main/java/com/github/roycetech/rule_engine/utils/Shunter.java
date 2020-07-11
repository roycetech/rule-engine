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
    private final Deque<String> stackOperations;

    /** Stack for holding expression converted to reversed polish notation. */
    private final Deque<String> stackRPN;

//    /** Stack for holding the calculations result. */
//    private final Deque<String> stackAnswer;

    public Shunter(final Deque<String> stackOperations,
	    final Deque<String> stackRPN) {
	this.stackOperations = stackOperations;
	this.stackRPN = stackRPN;
    }

    /** @param tokenChar token. */
    public void shuntInternal(final String token)
    {
	final char tokenChar = token.charAt(0);

	if (LogicHelper.isOpenBracket(tokenChar)) {
	    this.stackOperations.add(token);
	} else if (LogicHelper.isCloseBracket(tokenChar)) {
	    shuntClose();
	} else if (LogicHelper.isOperator(token)) {
	    shuntOperator(token);
	} else {
	    this.stackRPN.add(token);
	}
    }

    private void shuntClose()
    {
	while (!this.stackOperations.isEmpty() && !LogicHelper.isOpenBracket(
		this.stackOperations.peekLast().trim().charAt(0))) {
	    this.stackRPN.add(this.stackOperations.pop());
	}
	this.stackOperations.pop();
    }

    private void shuntOperator(final String token)
    {
	while (!this.stackOperations.isEmpty()
		&& LogicHelper
			.isOperator(this.stackOperations.peekLast().trim())
		&& Operator.fromString(token).getPrecedence() <= Operator
			.fromString(this.stackOperations.peekLast())
			.getPrecedence()) {
	    this.stackRPN.add(this.stackOperations.pop());
	}
	this.stackOperations.add(token);
    }

}
