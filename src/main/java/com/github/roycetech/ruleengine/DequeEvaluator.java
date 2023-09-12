/**
 *
 */
package com.github.roycetech.ruleengine;

import java.lang.reflect.Array;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.github.roycetech.ruleengine.converter.ElementConverter;
import com.github.roycetech.ruleengine.utils.ReflectionHelper;

/**
 * Helper class for RuleEvaluator.
 *
 * @author royce
 */
public class DequeEvaluator {

	/** Token based converters */
	private final Map<String, ElementConverter> tokenConverters;

	/** Stack for holding expression converted to reversed polish notation. */
	private final Deque<Object> stackRPN;

	/**
	 * Stack for holding the calculations result. Non-final for testability. Value
	 * type can be the traditional String type, but can also be an array.
	 */
	private Deque<Object> stackAnswer;

	/* default */ DequeEvaluator(final Deque<Object> stackRPN,
			final Map<String, ElementConverter> tokenConverters) {

		this.stackRPN = stackRPN;
		this.stackAnswer = new ArrayDeque<>();
		this.tokenConverters = tokenConverters;
	}

	/**
	 * @param scenario to evaluate against the rule expression.
	 */
	/* default */ Boolean evaluateOneRpn(final List<Object> scenario)
	{
		final String single = this.stackRPN.peek().toString();
		final ElementConverter converter = RuleEvaluator.TYPE_CONVERTER
				.get(scenario.get(0).getClass());

		final Token token = new Token(single);
		return token.accepts(scenario, converter);
	}

	/**
	 * @param scenario List of values to evaluate against the rule expression.
	 */
	/* default */ Boolean evaluateMultiRpn(final List<Object> scenario)
	{
		this.stackAnswer.clear();
		final Deque<Object> stackRPNClone = ((ArrayDeque<Object>) this.stackRPN).clone();

		/* Debug WorkerTest failure here. */
		evaluateStackRpn(stackRPNClone, scenario);

		final boolean multiAnswers = this.stackAnswer.size() > 1;
		if (multiAnswers) {
			throw new RuleEvaluatorException("Some operator is missing");
		}

		final String last = (String) this.stackAnswer.removeLast();
		return Boolean.valueOf(last.substring(1));
	}

	/**
	 * evaluating the RPN expression
	 *
	 * Package private for testability.
	 */
	/* default */ void evaluateStackRpn(final Deque<Object> stackRpn, final List<Object> scenario)
	{
		String token;
		while (!stackRpn.isEmpty()) {
			token = stackRpn.removeLast().toString();
			if (LogicHelper.isOperator(token)) {
				final char operatorChar = token.charAt(0);
				evaluateOperator(scenario, operatorChar);
			} else {
				this.stackAnswer.add(token);
			}
		}
	}

	private void evaluateOperator(final List<Object> scenario, final char tokenChar)
	{
		if (Operator.NOT.getSymbol() == tokenChar) {
			evaluateMultiNot(scenario);
		} else {
			final Operator operator = Operator.fromChar(tokenChar);
			evaluateMulti(scenario, operator);
		}
	}

	/**
	 *
	 * @param scenario List of values to evaluate against the rule expression.
	 *
	 *                 Package private for testability.
	 */
	/* default */ void evaluateMultiNot(final List<Object> scenario)
	{
		final String latest = this.stackAnswer.removeLast().toString().trim();
		String answer;

		if (LogicHelper.isTrue(latest)) {
			answer = LogicHelper.IFALSE;
		} else if (LogicHelper.isFalse(latest)) {
			answer = LogicHelper.ITRUE;
		} else {
			answer = evaluateNonInternal(scenario, latest);
		}

		this.stackAnswer.add(LogicHelper.formatInternalResult(answer));
	}

	/**
	 * Refactored method out of #evaluateMultiNot.
	 *
	 * @param scenario List of values to evaluate against the rule expression.
	 * @param latest   raw token to be evaluated.
	 *
	 * @return boolean result represented as String.
	 */
	private String evaluateNonInternal(final List<Object> scenario, final String latest)
	{
		final Token token = new Token(latest);
		final ElementConverter converter = RuleEvaluator.TYPE_CONVERTER
				.get(scenario.get(0).getClass());
		return String.valueOf(!token.accepts(scenario, converter));
	}

	/**
	 * @param scenario           List of values to evaluate against the rule
	 *                           expression.
	 * @param rule_token_convert token to converter map.
	 * @param operator           OR/AND.
	 */
	private void evaluateMulti(final List<Object> scenario, final Operator operator)
	{
		/* Convert "null" to null. */
		final List<Object> formattedScenario = scenario.stream()
				.map(s -> "null".equals(s) ? null : s).collect(Collectors.toList());

		final Token left = nextValue();
		final Token right = nextValue();

		final Class<?> tokenArray = Array.newInstance(Token.class, 0).getClass();

		final ReflectionHelper reflect = new ReflectionHelper(LogicHelper.class, "performLogical",
				List.class, tokenArray, Operator.class);

		final String answer = String.valueOf(reflect.<String>invoke(null, formattedScenario,
				new Token[] { left, right }, operator));

		this.stackAnswer.add(LogicHelper.formatInternalResult(answer));
	}

	/**
	 * @param rule_token_convert token to converter map.
	 * @param default_converter  default converter to use.
	 */
	private Token nextValue()
	{
		final Object lastAnswer = this.stackAnswer.removeLast();

		if (lastAnswer.getClass().isArray() || LogicHelper.isInternal((String) lastAnswer)) {
			return new Token(lastAnswer, -1);
		}

		return nextValueDefault((String) lastAnswer);
	}

	private Token nextValueDefault(final String tokenString)
	{
		final Token token = new Token(tokenString);
		final ElementConverter converter = this.tokenConverters == null
				? RuleEvaluator.TYPE_CONVERTER.get(tokenString.getClass())
				: this.tokenConverters.get(token.getValue());

		if (converter == null) {
			throw new RuleEvaluatorException(
					String.format("Config Error: Outcome clause token: '%s' not found in variables",
							token.getValue()));
		}

		token.convert(converter);
		return token;
	}

	/**
	 * For testability only.
	 *
	 * @return the stackAnswer
	 */
	/* default */ Deque<Object> getStackAnswer()
	{
		return stackAnswer;
	}

	/**
	 * @param stackAnswer the stackAnswer to set
	 */
	/* default */ void setStackAnswer(final Deque<Object> stackAnswer)
	{
		this.stackAnswer = stackAnswer;
	}
}
