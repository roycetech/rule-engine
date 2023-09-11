/**
 *
 */
package com.github.roycetech.rule_engine;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.github.roycetech.converter.BoolConverter;
import com.github.roycetech.converter.ElementConverter;
import com.github.roycetech.converter.FloatConverter;
import com.github.roycetech.converter.IntConverter;
import com.github.roycetech.converter.StrConverter;
import com.github.roycetech.rule_engine.utils.Shunter;
import com.github.roycetech.rule_engine.utils.TokenizerUtil;

/**
 * @author royce
 */
public class RuleEvaluator {

	/**
	 * Helper class to handle evaluations. Non-final for easier testability.
	 */
	private DequeEvaluator dequeEvaluator;

	/**
	 * Shunting-yard algorithm util
	 */
	private final Shunter shunter;

	/** list of available operators. */
	public static final String OPERATORS = String.format("%s%s%s", Operator.NOT.getSymbol(),
			Operator.OR.getSymbol(), Operator.AND.getSymbol());

	/**
	 * Converters for the different supported data types.
	 */
	/* default */ static final Map<Class<? extends Object>, ElementConverter> TYPE_CONVERTER;
	static {
		TYPE_CONVERTER = new HashMap<>();

		TYPE_CONVERTER.put(Integer.class, new IntConverter());
		TYPE_CONVERTER.put(Boolean.class, new BoolConverter());
		TYPE_CONVERTER.put(String.class, new StrConverter());
		TYPE_CONVERTER.put(Float.class, new FloatConverter());
		// array handled especially
	}

	/**
	 * List because this is required for index based token retrieval.
	 *
	 * @param tokenConverters token to converter mapping.
	 */
	public RuleEvaluator(final Map<String, ElementConverter> tokenConverters) {
		final Deque<Object> stackRPN = new ArrayDeque<>();
		this.dequeEvaluator = new DequeEvaluator(stackRPN, tokenConverters);

		this.shunter = new Shunter(new ArrayDeque<>(), stackRPN);
	}

	/**
	 * Parses the math expression (complicated formula) and builds the stack RPN to
	 * be used to evaluate against a scenario.
	 *
	 * @param expression input expression (logical expression formula). It can be a
	 *                   string or an array of objects.
	 * @since 0.3.0
	 */
	public void parse(final Object expression)
	{
		this.shunter.clearStacks();

		final Object tokens;
		if (expression.getClass().isArray()) {
			tokens = expression;
		} else {
			tokens = TokenizerUtil.tokenize((String) expression, OPERATORS + "()");
		}

		for (final Object token : (Object[]) tokens) {
			getShunter().shuntInternal(token);
		}

		getShunter().transferOperationsToRPN();
		// Reversed already.
	}

	/**
	 * Evaluates once parsed math expression with "var" variable included.
	 *
	 * @param scenario List of values to evaluate against the rule expression.
	 * @return <code>String</code> representation of the result
	 */
	public Boolean evaluate(final List<Object> scenario)
	{
		if (this.shunter.isOneRemaining()) {
			return getDequeEvaluator().evaluateOneRpn(scenario);
		}

		return getDequeEvaluator().evaluateMultiRpn(scenario);
	}

	/**
	 * @return the dequeEvaluator
	 */
	public DequeEvaluator getDequeEvaluator()
	{
		return dequeEvaluator;
	}

	/**
	 * @param dequeEvaluator the dequeEvaluator to set
	 */
	/* default */ void setDequeEvaluator(final DequeEvaluator dequeEvaluator)
	{
		this.dequeEvaluator = dequeEvaluator;
	}

	/**
	 * @return the shunter
	 */
	public Shunter getShunter()
	{
		return shunter;
	}
}
