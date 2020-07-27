package com.github.roycetech.rule_engine;

import java.util.Locale;

/** */
public enum Operator {

    /** rtfc. */
    NOT('!', Byte.MAX_VALUE),
    /**
     * rtfc.
     */
    AND('&', (byte) 2),

    /**
     * rtfc.
     */
    OR('|', (byte) 1);

    /** */
    private final char symbol;

    /** */
    private final byte precedence;

    /**
     *
     * @param pOperator   operator symbol character.
     * @param pPrecedence precedence for expression evaluation.
     */
    Operator(final char symbol, final byte pPrecedence) {
	this.symbol = symbol;
	this.precedence = pPrecedence;
    }

    /**
     * Derive Operator instance from a given character.
     *
     * @param symbol can be a character of bang, and, or.
     * @return the operator object for the given symbol.
     */
    public static Operator fromChar(final char symbol)
    {
	for (final Operator nextOper : Operator.values()) {
	    if (symbol == nextOper.symbol) {
		return nextOper;
	    }
	}
	throw new IllegalArgumentException(
		"The operator " + symbol + " is not supported.");
    }

    /**
     * Derive Operator instance from a given String.
     *
     * @param operator operator character symbol.
     * @return the operator object for the given symbol.
     */
    public static Operator fromString(final String operator)
    {
	for (final Operator nextOper : Operator.values()) {
	    if (operator.charAt(0) == nextOper.symbol) {
		return nextOper;
	    }
	}
	throw new IllegalArgumentException(
		"The operator " + operator + " is not supported.");
    }

    /**
     * @return the precedence
     */
    public byte getPrecedence()
    {
	return this.precedence;
    }

    /**
     * @return the precedence
     */
    public char getSymbol()
    {
	return this.symbol;
    }

    /**
     * Converts this operator to start with capital letter follow by lower case
     * for dynamic method invocation.
     *
     * @return word name of this operator.
     */
    public String toWord()
    {
	return name().substring(0, 1).toUpperCase(Locale.getDefault())
		+ name().substring(1).toLowerCase(Locale.getDefault());
    }
}
