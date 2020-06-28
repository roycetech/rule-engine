package com.github.roycetech.rule_engine;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class OperatorTest {

    @Test
    public final void testFromChar() {
	assertEquals(Operator.AND, Operator.fromChar('&'));
	assertEquals(Operator.OR, Operator.fromChar('|'));
	assertEquals(Operator.NOT, Operator.fromChar('!'));
    }

    @Test(expected = IllegalArgumentException.class)
    public final void testFromCharException() {
	Operator.fromChar('a');
    }

    @Test
    public final void testGetPrecedence() {
	assertEquals(true,
		Operator.NOT.getPrecedence() > Operator.AND.getPrecedence());

	assertEquals(true,
		Operator.AND.getPrecedence() > Operator.OR.getPrecedence());
    }

    @Test
    public final void testToString() {
	assertEquals("Not", Operator.NOT.toWord());
	assertEquals("And", Operator.AND.toWord());
	assertEquals("Or", Operator.OR.toWord());
    }
}
