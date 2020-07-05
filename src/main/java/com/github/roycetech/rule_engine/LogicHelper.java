/**
 * Subscript is synonymous to Index, shortened as idx.
 * Token is different from token. Small letter case is the general word, the
 * the Capitalize represent the object that contains the value and the
 * subscript.
 */
package com.github.roycetech.rule_engine;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Provides logic evaluation functionalities. Logical and/or/not evaluator.
 *
 * @author royce
 */
public class LogicHelper {

    /** Internal true result */
    static final String TRUE = "*true";

    /** Internal false result */
    static final String FALSE = "*false";

    /** Opposing result for internal boolean result. */
    static final Map<String, String> OPPOSITE = new HashMap<>();
    static {
	OPPOSITE.put(TRUE, FALSE);
	OPPOSITE.put(FALSE, TRUE);
    }

    /**
     * Defines the primary result for a logical operator.
     */
    static final EnumMap<Operator, String> PRIMARY_RESULT = new EnumMap<>(
	    Operator.class);
    static {
	PRIMARY_RESULT.put(Operator.AND, FALSE);
	PRIMARY_RESULT.put(Operator.OR, TRUE);
    }

    /**
     * Perform logical operation.
     *
     * @param scenario  the list of scenario tokens.
     * @param left      the left token object.
     * @param right     the right token object.
     * @param operation either AND or OR.
     *
     * @return the result of the logical operation.
     */
    public String performLogical(final List<Object> scenario, final Token left,
	    final Token right, final Operator operation)
    {

	final String evaluated = isBothInternalS(left, right, operation);

	if (Boolean.valueOf(evaluated)) {
	    return evaluated;
	}

	final String defaultReturn = Operator.AND == operation ? TRUE : FALSE;

	if (left.equalsInternal(defaultReturn)) {
	    return String.valueOf(right.accepts(scenario));
	}

	if (right.equalsInternal(defaultReturn)) {
	    return String.valueOf(left.accepts(scenario));
	}

	final String methodName = "evaluate" + operation.toWord();
	Method method;
	try {
	    method = getClass().getDeclaredMethod(methodName, List.class,
		    Token.class, Token.class);
	    return (String) method.invoke(this, scenario, left, right);
	} catch (NoSuchMethodException | SecurityException
		| IllegalAccessException | IllegalArgumentException
		| InvocationTargetException e) {
	    e.printStackTrace();
	    return null;
	}
    }

    static boolean isInternal(final String token)
    {
	return TRUE.equals(token) || FALSE.equals(token);
    }

//private methods ==========================================================

    /**
     * Check if the token is opening bracket.
     *
     * @token Input <code>String</code> token
     * @return <code>boolean</code> output
     */
    static boolean isOpenBracket(final char token)
    {
	return token == '(';
    }

    /**
     * Check if the token is closing bracket. # * # * @param token Input
     * <code>String</code> token # * @return <code>boolean</code> output #
     */
    static boolean isCloseBracket(final char token)
    {
	return token == ')';
    }

    /**
     * @param left     token.
     * @param right    token.
     * @param operator either 'and' or 'or' operator.
     */
    private String isBothInternalS(final Token left, final Token right,
	    final Operator operator)
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
    @SuppressWarnings("unused")
    private boolean evaluateAnd(final List<Object> scenario, final Token left,
	    final Token right)
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
    @SuppressWarnings("unused")
    private boolean evaluateOr(final List<Object> scenario, final Token left,
	    final Token right)
    {
	final boolean leftEval = left.accepts(scenario);
	if (leftEval) {
	    return true;
	}

	return right.accepts(scenario);
    }
}
