package com.github.roycetech.rule_engine;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.github.roycetech.converter.BooleanConverter;
import com.github.roycetech.converter.ElementConverter;
import com.github.roycetech.converter.FloatConverter;
import com.github.roycetech.converter.IntegerConverter;
import com.github.roycetech.converter.StringConverter;
import com.github.roycetech.rule_engine.utils.Shunter;
import com.github.roycetech.rule_engine.utils.TokenizerUtil;

/**
 * The `RuleEvaluator` class is responsible for parsing and evaluating logical
 * expressions. It supports mathematical formulas and boolean conditions against
 * a given scenario.
 *
 * <p>
 * This class provides a flexible and extensible way to evaluate complex
 * expressions with support for various operators and data types.
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

	/** List of available operators. */
	public static final String OPERATORS = String.format("%s%s%s", Operator.NOT.getSymbol(),
			Operator.OR.getSymbol(), Operator.AND.getSymbol());

	/**
	 * Converters for the different supported data types.
	 */
	/* default */ static final Map<Class<? extends Object>, ElementConverter> TYPE_CONVERTER;
	static {
		TYPE_CONVERTER = new HashMap<>();

		TYPE_CONVERTER.put(Integer.class, new IntegerConverter());
		TYPE_CONVERTER.put(Boolean.class, new BooleanConverter());
		TYPE_CONVERTER.put(String.class, new StringConverter());
		TYPE_CONVERTER.put(Float.class, new FloatConverter());
		// array handled especially
	}

	/**
	 * Creates a new `RuleEvaluator` instance with the specified token-to-converter
	 * mapping.
	 *
	 * @param tokenConverters The mapping of tokens to their corresponding element
	 *                        converters.
	 */
	public RuleEvaluator(final Map<String, ElementConverter> tokenConverters) {
		final Deque<Object> stackRPN = new ArrayDeque<>();
		this.dequeEvaluator = new DequeEvaluator(stackRPN, tokenConverters);

		this.shunter = new Shunter(new ArrayDeque<>(), stackRPN);
	}

	/**
	 * Parses the given mathematical or logical expression and constructs a reverse
	 * Polish notation (RPN) stack for evaluation against a scenario.
	 *
	 * @param expression The input expression, which can be a string or an array of
	 *                   objects.
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
	 * Evaluates the parsed expression with a given scenario and returns the result.
	 *
	 * @param scenario The list of values to evaluate against the rule expression.
	 * @return The result of the evaluation, represented as a Boolean value.
	 */
	public Boolean evaluate(final List<Object> scenario)
	{
		if (this.shunter.isOneRemaining()) {
			return getDequeEvaluator().evaluateOneRpn(scenario);
		}

		return getDequeEvaluator().evaluateMultiRpn(scenario);
	}

	/**
	 * Gets the `DequeEvaluator` instance used for handling evaluations.
	 *
	 * @return The `DequeEvaluator` instance responsible for handling evaluations.
	 */
	public DequeEvaluator getDequeEvaluator()
	{
		return dequeEvaluator;
	}

	/**
	 * Sets the `DequeEvaluator` instance used for handling evaluations. This method
	 * is package-private to allow for easier testability.
	 *
	 * @param dequeEvaluator The `DequeEvaluator` instance to set.
	 */
	/* default */ void setDequeEvaluator(final DequeEvaluator dequeEvaluator)
	{
		this.dequeEvaluator = dequeEvaluator;
	}

	/**
	 * Gets the `Shunter` instance used for the Shunting-yard algorithm.
	 *
	 * @return The `Shunter` instance responsible for performing the Shunting-yard
	 *         algorithm.
	 */
	public Shunter getShunter()
	{
		return shunter;
	}
}
