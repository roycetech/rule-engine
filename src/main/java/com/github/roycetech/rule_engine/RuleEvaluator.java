/**
 *
 */
package com.github.roycetech.rule_engine;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.github.roycetech.converter.BoolConverter;
import com.github.roycetech.converter.ElementConverter;
import com.github.roycetech.converter.FloatConverter;
import com.github.roycetech.converter.IntConverter;
import com.github.roycetech.converter.StrConverter;
import com.github.roycetech.rule_engine.utils.ClauseTokenizer;
import com.github.roycetech.rule_engine.utils.Shunter;

/**
 * @author royce
 */
public class RuleEvaluator {

    /**
     * Helper class to handle evaluations.
     */
    private final DequeEvaluator dequeEvaluator;

    /**
     * Shunting-yard algorithm util
     */
    private final Shunter shunter;

    /** Temporary stack that holds operators, functions and brackets. */
    private final Deque<String> stackOperations;

    /** Stack for holding expression converted to reversed polish notation. */
    private final Deque<String> stackRPN;

    /** Stack for holding the calculations result. */
    private final Deque<String> stackAnswer;

    /** list of available operators. */
    public static final String OPERATORS = String.format("%s%s%s", Operator.NOT,
	    Operator.OR, Operator.AND);

    /**
     * Converters for the different supported data types.
     */
    private static final Map<Class<? extends Object>, ElementConverter<?>> TYPE_CONVERTER;
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
    public RuleEvaluator(
	    final Map<String, ElementConverter<?>> tokenConverters) {
	this.stackOperations = new ArrayDeque<>();
	this.stackRPN = new ArrayDeque<>();
	this.stackAnswer = new ArrayDeque<>();

	this.dequeEvaluator = new DequeEvaluator(this.stackRPN,
		this.stackAnswer, tokenConverters);

	this.shunter = new Shunter(this.stackOperations, this.stackAnswer);
    }

    /**
     * Parses the math expression (complicated formula) and stores the result.
     *
     * @param expression input expression (logical expression formula). It can
     *                   be a string or an array of objects.
     * @since 0.3.0
     */
    void parse(final Object expression)
    {
	/* cleaning stacks */
	this.stackOperations.clear();
	this.stackRPN.clear();

	final Object tokens;
	if (expression.getClass().isArray()) {
	    tokens = expression;
	} else {
	    tokens = ClauseTokenizer.tokenize((String) expression,
		    RuleEvaluator.OPERATORS + "()");
	}

	for (final String token : (String[]) tokens) {
	    getShunter().shuntInternal(token);
	}

	while (!this.stackOperations.isEmpty()) {
	    this.stackRPN.add(this.stackOperations.pop());
	}

	Collections.reverse((List<?>) this.stackRPN);
    }

    /**
     * Evaluates once parsed math expression with "var" variable included.
     *
     * @param scenario List of values to evaluate against the rule expression.
     * @return <code>String</code> representation of the result
     */
    public Boolean evaluate(final List<Object> scenario)
    {
	final boolean oneLeft = this.stackRPN.size() == 1;
	if (oneLeft) {
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
     * @return the shunter
     */
    public Shunter getShunter()
    {
	return shunter;
    }
}

/** Specialized exception for parse error. */
class RuleEvaluatorException extends RuntimeException {

    /** @param string exception message. */
    RuleEvaluatorException(final String string) {
	super(string);
    }
}
