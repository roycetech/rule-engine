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
import com.github.roycetech.rule_engine.utils.TokenizerUtil;
import com.github.roycetech.rule_engine.utils.Shunter;

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

    /** Temporary stack that holds operators, functions and brackets. */
    private final Deque<Object> stackOperations;

    /**
     * Stack for holding expression converted to reversed polish notation.
     * Non-final for testability.
     */
    private Deque<Object> stackRPN;

    /** list of available operators. */
    public static final String OPERATORS = String.format("%s%s%s",
	    Operator.NOT.getSymbol(), Operator.OR.getSymbol(),
	    Operator.AND.getSymbol());

    /**
     * Converters for the different supported data types.
     */
    static final Map<Class<? extends Object>, ElementConverter> TYPE_CONVERTER;
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
	this.stackOperations = new ArrayDeque<>();
	this.stackRPN = new ArrayDeque<>();
	this.dequeEvaluator = new DequeEvaluator(this.stackRPN,
		tokenConverters);

	this.shunter = new Shunter(this.stackOperations, this.stackRPN);
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
	    tokens = TokenizerUtil.tokenize((String) expression,
		    RuleEvaluator.OPERATORS + "()");
	}

	for (final Object token : (Object[]) tokens) {
	    getShunter().shuntInternal(token);
	}

	while (!this.stackOperations.isEmpty()) {
	    this.stackRPN.push(this.stackOperations.removeLast());
	}

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
	final boolean oneLeft = this.stackRPN.size() == 1;
	if (oneLeft) {
	    return getDequeEvaluator().evaluateOneRpn(scenario);
	}

	return getDequeEvaluator().evaluateMultiRpn(scenario);
    }

    /**
     * @return the stackRPN
     */
    Deque<Object> getStackRPN()
    {
	return stackRPN;
    }

    /**
     * @param stackRPN the stackRPN to set
     */
    void setStackRPN(final Deque<Object> stackRPN)
    {
	this.stackRPN = stackRPN;
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
    void setDequeEvaluator(final DequeEvaluator dequeEvaluator)
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

/** Specialized exception for parse error. */
class RuleEvaluatorException extends RuntimeException {

    /** @param string exception message. */
    RuleEvaluatorException(final String string) {
	super(string);
    }
}
