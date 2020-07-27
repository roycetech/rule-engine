/**
 *
 */
package com.github.roycetech.rule_engine;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.roycetech.converter.BoolConverter;
import com.github.roycetech.converter.ElementConverter;
import com.github.roycetech.converter.FloatConverter;
import com.github.roycetech.converter.IntConverter;
import com.github.roycetech.converter.StrConverter;

/**
 * Helper class for RuleEvaluator.
 *
 * @author royce
 */
public class DequeEvaluator {

    /** */
    private static final Logger LOGGER = LoggerFactory
	    .getLogger(DequeEvaluator.class);

    /** Token based converters */
    private final Map<String, ElementConverter> tokenConverters;

    /** Stack for holding expression converted to reversed polish notation. */
    private final Deque<Object> stackRPN;

    /**
     * Stack for holding the calculations result. Non-final for testability.
     * Value type can be the traditional String type, but can also be an array.
     */
    private Deque<Object> stackAnswer;

    /**
     * Converters for the different supported data types.
     */
    private static final Map<Class<? extends Object>, ElementConverter> TYPE_CONVERTER;
    static {
	TYPE_CONVERTER = new HashMap<>();

	TYPE_CONVERTER.put(Integer.class, new IntConverter());
	TYPE_CONVERTER.put(Boolean.class, new BoolConverter());
	TYPE_CONVERTER.put(String.class, new StrConverter());
	TYPE_CONVERTER.put(Float.class, new FloatConverter());
	// array handled especially
    }

    DequeEvaluator(final Deque<Object> stackRPN,
	    final Map<String, ElementConverter> tokenConverters) {

	this.stackRPN = stackRPN;
	this.stackAnswer = new ArrayDeque<>();
	this.tokenConverters = tokenConverters;
    }

    /**
     * @param scenario to evaluate against the rule expression.
     */
    Boolean evaluateOneRpn(final List<Object> scenario)
    {
	final String single = this.stackRPN.peek().toString();
	final ElementConverter converter = TYPE_CONVERTER
		.get(scenario.get(0).getClass());

	final Token token = new Token(single);
	return token.accepts(scenario, converter);
    }

    /**
     * @param scenario List of values to evaluate against the rule expression.
     */
    Boolean evaluateMultiRpn(final List<Object> scenario)
    {
	this.stackAnswer.clear();
	final Deque<Object> stackRPNClone = ((ArrayDeque<Object>) this.stackRPN)
		.clone();

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
    void evaluateStackRpn(final Deque<Object> stackRpn,
	    final List<Object> scenario)
    {
	String token;
	while (!stackRpn.isEmpty()) {
	    token = stackRpn.removeLast().toString();
	    if (LogicHelper.isOperator(token)) {
		evaluateOperator(scenario, token.charAt(0));
	    } else {
		this.stackAnswer.push(token);
	    }
	}
    }

    private void evaluateOperator(final List<Object> scenario,
	    final char tokenChar)
    {
	if (Operator.NOT.getSymbol() == tokenChar) {
	    evaluateMultiNot(scenario);
	} else {
	    evaluateMulti(scenario, Operator.fromChar(tokenChar));
	}
    }

    /**
     *
     * @param scenario List of values to evaluate against the rule expression.
     *
     *                 Package private for testability.
     */
    void evaluateMultiNot(final List<Object> scenario)
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
    private String evaluateNonInternal(final List<Object> scenario,
	    final String latest)
    {
	final Token token = new Token(latest);
	final ElementConverter converter = TYPE_CONVERTER
		.get(scenario.get(0).getClass());
	return String.valueOf(!token.accepts(scenario, converter));
    }

    /**
     * @param scenario           List of values to evaluate against the rule
     *                           expression.
     * @param rule_token_convert token to converter map.
     * @param operator           OR/AND.
     */
    private void evaluateMulti(final List<Object> scenario,
	    final Operator operator)
    {
	/* Convert "null" to null. */
	final List<Object> formattedScenario = scenario.stream()
		.map(s -> "null".equals(s) ? null : s)
		.collect(Collectors.toList());

	final Token left = nextValue();
	final Token right = nextValue();

	try {
	    final Class<?> tokenArray = Array.newInstance(Token.class, 0)
		    .getClass();

	    final Method method = LogicHelper.class.getDeclaredMethod(
		    "performLogical", List.class, tokenArray, Operator.class);

	    final String answer = String.valueOf(method.invoke(null,
		    formattedScenario, new Token[] { left, right
		    }, operator));

	    this.stackAnswer.push(LogicHelper.formatInternalResult(answer));
	} catch (final SecurityException | NoSuchMethodException
		| IllegalArgumentException | IllegalAccessException
		| InvocationTargetException e) {
	    LOGGER.error(e.getMessage(), e);
	}
    }

    /**
     * @param rule_token_convert token to converter map.
     * @param default_converter  default converter to use.
     */
    private Token nextValue()
    {
	final Object lastAnswer = this.stackAnswer.removeLast();

	if (lastAnswer.getClass().isArray()
		|| LogicHelper.isInternal((String) lastAnswer)) {
	    return new Token(lastAnswer, -1);
	}

	return nextValueDefault((String) lastAnswer);
    }

    private Token nextValueDefault(final String tokenString)
    {
	final Token token = new Token(tokenString);

	final ElementConverter converter = this.tokenConverters
		.get(token.getValue());
	if (converter == null) {
	    throw new RuleEvaluatorException(String.format(
		    "Config Error: Outcome clause token: '%s' not found in variables",
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
    Deque<Object> getStackAnswer()
    {
	return stackAnswer;
    }

    /**
     * @param stackAnswer the stackAnswer to set
     */
    void setStackAnswer(final Deque<Object> stackAnswer)
    {
	this.stackAnswer = stackAnswer;
    }
}
