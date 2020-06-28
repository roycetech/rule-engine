package com.github.roycetech.rule_engine;

/** */
enum Operator {
    /** */
    NOT('!', Byte.MAX_VALUE), AND('&', (byte) 2), OR('|', (byte) 1);

    /** */
    private final char operator;

    /** */
    private final byte precedence;

    /**
     *
     * @param pOperator   operator symbol character.
     * @param pPrecedence precedence for expression evaluation.
     */
    Operator(final char pOperator, final byte pPrecedence) {
	this.operator = pOperator;
	this.precedence = pPrecedence;
    }

    /**
     * Derive Operator instance from a given character.
     *
     * @param operator operator character symbol.
     */
    public static Operator fromChar(final char operator) {
	for (final Operator nextOper : Operator.values()) {
	    if (operator == nextOper.operator) {
		return nextOper;
	    }
	}
	throw new IllegalArgumentException(
		"The operator " + operator + " is not supported.");
    }

    /**
     * @return the precedence
     */
    public byte getPrecedence() {
	return this.precedence;
    }

//    /** String representation of this object. */
//    @Override
//    public String toString() {
//	return String.valueOf(this.operator);
//    }

    /**
     * Converts this operator to start with capital letter follow by lower case
     * for dynamic method invocation.
     *
     * @return word name of this operator.
     */
    public String toWord() {
	return name().substring(0, 1).toUpperCase()
		+ name().substring(1).toLowerCase();
    }
}
