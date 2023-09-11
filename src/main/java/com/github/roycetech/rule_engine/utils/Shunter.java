/**
 *
 */
package com.github.roycetech.rule_engine.utils;

import java.util.Deque;

import com.github.roycetech.rule_engine.LogicHelper;
import com.github.roycetech.rule_engine.Operator;

/**
 * The `Shunter` class applies the Shunting Yard algorithm for parsing and
 * converting mathematical and logical expressions into Reverse Polish Notation
 * (RPN).
 *
 * <p>
 * It is used to handle the shunting yard algorithm, which is a method for
 * parsing expressions specified in infix notation (e.g., 3 + 4) into Reverse
 * Polish Notation (RPN) (e.g., 3 4 +).
 *
 * <p>
 * The class manages two stacks: one for operators and one for the expression in
 * RPN form.
 *
 * @author royce
 */
public class Shunter {

	/** Temporary stack that holds operators, functions and brackets. */
	private final Deque<Object> stackOperations;

	/** Stack for holding expressions converted to Reverse Polish Notation (RPN). */
	private Deque<Object> stackRPN;

	/**
	 * Constructs a `Shunter` instance with the specified stacks.
	 *
	 * @param stackOperations The stack to hold operators, functions, and brackets.
	 * @param stackRPN        The stack for holding expressions in Reverse Polish
	 *                        Notation (RPN).
	 */
	public Shunter(final Deque<Object> stackOperations, final Deque<Object> stackRPN) {
		this.stackOperations = stackOperations;
		this.stackRPN = stackRPN;
	}

	/**
	 * Performs the shunting yard algorithm on the provided token and updates the
	 * stacks accordingly.
	 *
	 * @param token The token to be processed by the shunting yard algorithm.
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

	/**
	 * Checks if there is only one remaining item in the RPN stack.
	 *
	 * @return `true` if there is only one remaining item in the RPN stack;
	 *         otherwise, `false`.
	 */
	public boolean isOneRemaining()
	{
		return this.stackRPN.size() == 1;
	}

	/**
	 * Clears both the operations and RPN stacks.
	 */
	public void clearStacks()
	{
		this.stackOperations.clear();
		this.stackRPN.clear();
	}

	/**
	 * Transfers all operations from the operations stack to the RPN stack.
	 */
	public void transferOperationsToRPN()
	{
		while (!this.stackOperations.isEmpty()) {
			this.stackRPN.push(this.stackOperations.removeLast());
		}
	}

	/**
	 * Gets the RPN stack for testing purposes.
	 *
	 * @return The RPN stack.
	 */
	public Deque<Object> getStackRPN()
	{
		return stackRPN;
	}

	/**
	 * Sets the RPN stack for testing purposes.
	 *
	 * @param stackRPN The RPN stack to set.
	 */
	public void setStackRPN(final Deque<Object> stackRPN)
	{
		this.stackRPN = stackRPN;
	}

	/**
	 * Gets the operations stack for testing purposes.
	 *
	 * @return The operations stack.
	 */
	Deque<Object> getStackOperations()
	{
		return stackOperations;
	}

	/**
	 * This method is used to handle closing brackets in the Shunting Yard
	 * algorithm. The algorithm works by popping operators from the stack until an
	 * opening bracket is found. This ensures that the operators are evaluated in
	 * the correct order.
	 */
	private void shuntClose()
	{
		while (!this.stackOperations.isEmpty() && !LogicHelper
				.isOpenBracket(this.stackOperations.peekLast().toString().trim().charAt(0))) {
			this.stackRPN.push(this.stackOperations.removeLast());
		}
		this.stackOperations.removeLast();
	}

	/**
	 * Handles the shunting of an operator token.
	 *
	 * @param token The operator token to be processed.
	 */
	private void shuntOperator(final Object token)
	{
		while (!this.stackOperations.isEmpty()
				&& LogicHelper.isOperator(this.stackOperations.peekLast().toString().trim())
				&& Operator.fromString(token.toString()).getPrecedence() <= Operator
						.fromString(this.stackOperations.peekLast().toString()).getPrecedence()) {
			this.stackRPN.addFirst(this.stackOperations.removeLast());
		}
		this.stackOperations.add(token);
	}
}
