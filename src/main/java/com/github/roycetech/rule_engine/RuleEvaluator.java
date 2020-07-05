/**
 *
 */
package com.github.roycetech.rule_engine;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.roycetech.converter.BoolConverter;
import com.github.roycetech.converter.ElementConverter;
import com.github.roycetech.converter.FloatConverter;
import com.github.roycetech.converter.IntConverter;
import com.github.roycetech.converter.StrConverter;

/**
 * @author royce
 *
 */
public class RuleEvaluator {

    /** */
    private static final Logger LOGGER = LoggerFactory
	    .getLogger(RuleEvaluator.class);

    /** Custom logical AND/OR evaluator. */
    private final transient LogicHelper logicHelper = new LogicHelper();

    /** Token based converters */
    private final Map<String, ElementConverter<?>> tokenConverters;

    /** Temporary stack that holds operators, functions and brackets. */
    private final Stack<String> stackOperations;

    /** Stack for holding expression converted to reversed polish notation. */
    private final Stack<String> stackRPN;

    /** Stack for holding the calculations result. */
    private final Stack<String> stackAnswer;

    /** list of available operators. */
    private static final String OPERATORS = Operator.NOT
	    + String.valueOf(Operator.OR) + Operator.AND;

    private static Map<Class<? extends Object>, ElementConverter<?>> DEFAULT_CONVERT_HASH;
    static {
	DEFAULT_CONVERT_HASH = new HashMap<>();

	DEFAULT_CONVERT_HASH.put(Integer.class, new IntConverter());
	DEFAULT_CONVERT_HASH.put(Boolean.class, new BoolConverter());
	DEFAULT_CONVERT_HASH.put(String.class, new StrConverter());
	DEFAULT_CONVERT_HASH.put(Float.class, new FloatConverter());
	// array handled especially
    }

    /**
     * List because this is required for index based token retrieval.
     *
     * @param tokenConverters token to converter mapping.
     */
    public RuleEvaluator(Map<String, ElementConverter<?>> tokenConverters) {
	this.tokenConverters = tokenConverters;

	this.stackOperations = new Stack<>();
	this.stackRPN = new Stack<>();
	this.stackAnswer = new Stack<>();
    }

    /**
     * Parses the math expression (complicated formula) and stores the result.
     *
     * @param expression input expression (logical expression formula). It can
     *                   be a string or an array of objects.
     * @since 0.3.0
     */
    void parse(Object expression)
    {
	/* cleaning stacks */
	this.stackOperations.clear();
	this.stackRPN.clear();

	final Object tokens;
	if (expression.getClass().isArray()) {
	    tokens = expression;
	} else {
	    tokens = RuleEvaluator.tokenize((String) expression);
	}

	for (final String token : (String[]) tokens) {
	    shuntInternal(token);
	}

	while (!this.stackOperations.empty()) {
	    this.stackRPN.add(this.stackOperations.pop());
	}

	Collections.reverse(this.stackRPN);
    }

    /**
     * Evaluates once parsed math expression with "var" variable included.
     *
     * @param scenario           List of values to evaluate against the rule
     *                           expression.
     * @param rule_token_convert mapping of rule tokens to converter.
     * @return <code>String</code> representation of the result
     */
    public Boolean evaluate(List<Object> scenario)
    {
	if (this.stackRPN.size() == 1) {
	    return evaluateOneRpn(scenario);
	}

	return evaluateMultiRpn(scenario);
    }

//    PRIVATE METHODS ========================================================

    /**
     * @clause - rule clause to be tokenized
     */
    private static List<String> tokenize(final String clause)
    {
	final StringTokenizer stringTokenizer = new StringTokenizer(clause,
		RuleEvaluator.OPERATORS + "()", true);

	final List<String> retval = new ArrayList<>();

	/* loop for handling each token - shunting-yard algorithm */
	while (stringTokenizer.hasMoreTokens()) {
	    final String token = stringTokenizer.nextToken().trim();
	    retval.add(token);
	}
	return retval;
    }

    /** @param tokenChar token. */
    private void shuntInternal(String token)
    {
	final char tokenChar = token.charAt(0);

	if (LogicHelper.isOpenBracket(tokenChar)) {
	    this.stackOperations.add(token);
	} else if (LogicHelper.isCloseBracket(tokenChar)) {
	    shuntClose();
	} else if (isOperator(token)) {
	    shuntOperator(token);
	} else {
	    this.stackRPN.add(token);
	}
    }

    private boolean isOperator(final String token)
    {
	return !"".equals(token) && RuleEvaluator.OPERATORS.contains(token);
    }

    private void shuntClose()
    {
	while (!this.stackOperations.isEmpty() && !LogicHelper.isOpenBracket(
		this.stackOperations.lastElement().trim().charAt(0))) {
	    this.stackRPN.add(this.stackOperations.pop());
	}
	this.stackOperations.pop();
    }

    private void shuntOperator(final String token)
    {
	while (!this.stackOperations.empty()
		&& isOperator(this.stackOperations.lastElement().trim())
		&& Operator.fromString(token).getPrecedence() <= Operator
			.fromString(this.stackOperations.lastElement())
			.getPrecedence()) {
	    this.stackRPN.add(this.stackOperations.pop());
	}
	this.stackOperations.add(token);
    }

    /**
     * @param scenario to evaluate against the rule expression.
     */
    private Boolean evaluateOneRpn(List<Object> scenario)
    {
	final String single = this.stackRPN.peek();
	final ElementConverter<?> converter = DEFAULT_CONVERT_HASH
		.get(scenario.get(0).getClass());

	final Token token = new Token(single);
	return token.accepts(scenario, converter);
    }

    /**
     * @param scenario List of values to evaluate against the rule expression.
     */
    private Boolean evaluateMultiRpn(final List<Object> scenario)
    {
	this.stackAnswer.clear();

	/* get the clone of the RPN stack for further evaluating */
	@SuppressWarnings("unchecked")
	final Stack<String> stackRPNClone = (Stack<String>) this.stackRPN
		.clone();

	evaluateStackRpn(stackRPNClone, scenario);

	if (this.stackAnswer.size() > 1) {
	    throw new RuleEvaluatorException("Some operator is missing");
	}

	return Boolean.valueOf(this.stackAnswer.pop().toString().substring(1));
    }

    /**
     * Returns value of 'n' if rule token ends with '[n]'. where 'n' is the
     * variable group index.
     *
     * @param string token to check for subscript.
     */
    static int extractSubscript(Object token)
    {
	if (token.getClass().isArray()) {
	    return -1;
	}

//      int subscript = token[/\[(\d+)\]$/, 1]
//      subscript.nil? ? -1 : subscript.to_i

	final String tokenString = (String) token;

	int retval = -1; // NOPMD: null default, conditionally redefine.
	final Pattern pattern = Pattern.compile(".*\\[[\\d*]\\]");
	final Matcher matcher = pattern.matcher(tokenString);
	if (matcher.find()) {
	    final String indexStr = tokenString.substring(
		    tokenString.indexOf('[') + 1, tokenString.indexOf(']'));
	    retval = Integer.parseInt(indexStr);
	}
	return retval;
    }

    /** evaluating the RPN expression */
    private void evaluateStackRpn(Stack<String> stackRpn, List<Object> scenario)
    {
	String token;
	while (!stackRpn.isEmpty()) {
	    token = stackRpn.pop();
	    if (isOperator(token)) {
		evaluateOperator(scenario, token.charAt(0));
	    } else {
		this.stackAnswer.add(token);
	    }
	}
    }

    private void evaluateOperator(List<Object> scenario, char tokenChar)
    {
	if (Operator.NOT.getSymbol() == tokenChar) {
	    evaluateMultiNot(scenario);
	} else {
	    evaluateMulti(scenario, Operator.fromChar(tokenChar));
	}
    }

    /**
     * @param scenario List of values to evaluate against the rule expression.
     */
    private void evaluateMultiNot(List<Object> scenario)
    {
	final String latest = this.stackAnswer.pop().trim();
	String answer;

	if (LogicHelper.TRUE == latest) {
	    answer = LogicHelper.FALSE;
	} else if (LogicHelper.FALSE == latest) {
	    answer = LogicHelper.TRUE;
	} else {
	    answer = evaluateNonInternal(scenario, latest);
	}

	this.stackAnswer.add(formatInternalResult(answer));
    }

    private String evaluateNonInternal(List<Object> scenario, String latest)
    {
	final Token token = new Token(latest);
	final ElementConverter<?> converter = DEFAULT_CONVERT_HASH
		.get(scenario.get(0).getClass());
	return String.valueOf(!token.accepts(scenario, converter));
    }

    /**
     * Returns true if answer starts with *, *true if answer is true, same goes
     * for false.
     */
    private String formatInternalResult(String answer)
    {
	if (answer.charAt(0) == '*') {
	    return answer;
	}
	return String.format("*%s", answer);
    }

    /**
     * @param scenario           List of values to evaluate against the rule
     *                           expression.
     * @param rule_token_convert token to converter map.
     * @param operator           OR/AND.
     */
    private void evaluateMulti(List<Object> scenario, Operator operator)
    {
	/* Convert 'nil' to nil. */
	final List<Object> formattedScenario = scenario.stream()
		.map(token -> "nil".equals(token)).collect(Collectors.toList());

	final Token left = nextValue();
	final Token right = nextValue();

	try {
	    final Method method = LogicHelper.class.getDeclaredMethod(
		    "performLogical",
		    new Class[] {
			    List.class,
			    Token.class,
			    Token.class,
			    Operator.class
		    });

	    final String answer = (String) method.invoke(logicHelper,
		    new Object[] { formattedScenario, left, right, operator
		    });

	    this.stackAnswer.add(formatInternalResult(answer));

	} catch (final SecurityException e1) {
	    RuleEvaluator.LOGGER.error(e1.getMessage(), e1);
	} catch (final NoSuchMethodException e1) {
	    RuleEvaluator.LOGGER.error(e1.getMessage(), e1);
	} catch (final IllegalArgumentException e) {
	    RuleEvaluator.LOGGER.error(e.getMessage(), e);
	} catch (final IllegalAccessException e) {
	    RuleEvaluator.LOGGER.error(e.getMessage(), e);
	} catch (final InvocationTargetException e) {
	    RuleEvaluator.LOGGER.error(e.getMessage(), e);
	}
    }

    /**
     * @param rule_token_convert token to converter map.
     * @param default_converter  default converter to use.
     */
    private Token nextValue()
    {
	final Object lastAnswer = this.stackAnswer.pop();

	if (lastAnswer.getClass().isArray()
		|| LogicHelper.isInternal((String) lastAnswer)) {
	    return new Token(lastAnswer, -1);
	}

	return nextValueDefault((String) lastAnswer);
    }

    private Token nextValueDefault(String tokenString)
    {
	final Token token = new Token(tokenString);

	final ElementConverter<?> converter = this.tokenConverters
		.get(token.getValue());
	if (converter == null) {
	    throw new RuleEvaluatorException(String.format(
		    "Config Error: Outcome clause token: '%s' not found in variables",
		    token.getValue()));
	}

	token.convert(converter);
	return token;
    }
}

/** Specialized exception for parse error. */
class RuleEvaluatorException extends RuntimeException {

    /** @param string exception message. */
    RuleEvaluatorException(final String string) {
	super(string);
    }
}
