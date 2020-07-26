/**
 * Subscript is synonymous to Index, shortened as idx.
 * Token is different from token. Small letter case is the general word, the
 * the capitalized represent the object that contains the value and the
 * subscript.
 */
package com.github.roycetech.rule_engine;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Internal refers to the string representation of the boolean values, signified
 * by the * prefix. Provides logic evaluation functionalities. Logical
 * and/or/not evaluator.
 *
 * @author royce
 */
public class LogicHelper {

    /** */
    private static final Logger LOGGER = LoggerFactory
	    .getLogger(LogicHelper.class);

    /** Internal true result */
    static final String ITRUE = "*true";

    /** Internal false result */
    static final String IFALSE = "*false";

    /** Opposing result for internal boolean result. */
    static final Map<String, String> OPPOSITE = new HashMap<>();
    static {
	OPPOSITE.put(ITRUE, IFALSE);
	OPPOSITE.put(IFALSE, ITRUE);
    }

    /**
     * Defines the primary result for a logical operator.
     */
    static final EnumMap<Operator, String> PRIMARY_RESULT = new EnumMap<>(
	    Operator.class);
    static {
	PRIMARY_RESULT.put(Operator.AND, IFALSE);
	PRIMARY_RESULT.put(Operator.OR, ITRUE);
    }

    private LogicHelper() {
    }

    /**
     * Perform logical operation. It may return internal boolean representation,
     * thus the return value is string.
     *
     * @param scenario  the list of scenario tokens.
     * @param tokenPair the left and right token for binary evaluation.
     * @param operation either AND or OR.
     * @return the result of the logical operation.
     */
    public static String performLogical(final List<Object> scenario,
	    final Token[] tokenPair, final Operator operation)
    {

	final Token left = tokenPair[0];
	final Token right = tokenPair[1];
	final String evaluated = performBothInternal(left, right, operation);

	if (!"false".equals(evaluated)) {
	    return evaluated;
	}

	final String defaultReturn = Operator.AND == operation ? ITRUE : IFALSE;

	if (left.equalsInternal(defaultReturn)) {
	    return String.valueOf(right.accepts(scenario));
	}

	if (right.equalsInternal(defaultReturn)) {
	    return String.valueOf(left.accepts(scenario));
	}

	final String methodName = "evaluate" + operation.toWord();
	Method method;
	try {
	    method = LogicHelper.class.getDeclaredMethod(methodName, List.class,
		    Token.class, Token.class);
	    return method.invoke(null, scenario, left, right).toString();
	} catch (NoSuchMethodException | SecurityException
		| IllegalAccessException | IllegalArgumentException
		| InvocationTargetException e) {
	    LOGGER.error(e.getMessage(), e);
	    return null;
	}
    }

    /**
     * Check if the token is opening bracket.
     *
     * @param token Input <code>String</code> token
     * @return <code>boolean</code> output
     */
    public static boolean isOpenBracket(final char token)
    {
	return token == '(';
    }

    /**
     * Check if the token is closing bracket.
     *
     * @param token the char to check for bracket.
     * @return true if the token is a close bracket.
     */
    public static boolean isCloseBracket(final char token)
    {
	return token == ')';
    }

    /**
     * Convenience method that checks if target represents internal true.
     *
     * @param target target to check.
     * @return true if target is internal true.
     */
    static boolean isTrue(final String target)
    {
	return ITRUE.equals(target);
    }

    /**
     * Convenience method that checks if target represents internal false.
     *
     * @param target target to check.
     * @return true if target is internal false.
     */
    static boolean isFalse(final String target)
    {
	return IFALSE.equals(target);
    }

    static boolean isInternal(final String token)
    {
	return ITRUE.equals(token) || IFALSE.equals(token);
    }

    public static boolean isOperator(final Object token)
    {
	if (token == null) {
	    return false;
	}

	final String tokenString = token.toString();
	return !"".equals(tokenString) && tokenString.length() == 1
		&& RuleEvaluator.OPERATORS.indexOf(tokenString) > -1;
    }

    /**
     * Returns the passed parameter if it already starts with *, *true if answer
     * is true, and *false for false.
     */
    static String formatInternalResult(final String answer)
    {
	final boolean isInternal = answer.charAt(0) == '*';
	if (isInternal) {
	    return answer;
	}
	return String.format("*%s", answer);
    }

//private methods ==========================================================

    /**
     * Evaluates when both the tokens are internal booleans.
     *
     * @param left     token.
     * @param right    token.
     * @param operator either 'and' or 'or' operator.
     */
    private static String performBothInternal(final Token left,
	    final Token right, final Operator operator)
    {
	final String defaultResult = PRIMARY_RESULT.get(operator);

	if (left.equalsInternal(defaultResult)
		|| right.equalsInternal(defaultResult)) {
	    return defaultResult;
	}

	final String opposite = OPPOSITE.get(defaultResult);
	if (left.equalsInternal(opposite) && right.equalsInternal(opposite)) {
	    return opposite;
	}

	return String.valueOf(false);
    }

    /**
     * This is invoked via reflection.
     *
     * @param scenario
     * @param left
     * @param right
     * @return
     */
    @SuppressWarnings("unused") // Used via reflection.
    private static boolean evaluateAnd(final List<Object> scenario,
	    final Token left, final Token right)
    {
	final boolean leftEval = left.accepts(scenario);

	if (!leftEval) {
	    return false;
	}

	return right.accepts(scenario);
    }

    /**
     * This is invoked via reflection.
     *
     * @param scenario
     * @param left
     * @param right
     * @return
     */
    @SuppressWarnings("unused") // Used via reflection.
    private static boolean evaluateOr(final List<Object> scenario,
	    final Token left, final Token right)
    {
	final boolean leftEval = left.accepts(scenario);
	if (leftEval) {
	    return true;
	}

	return right.accepts(scenario);
    }
}
