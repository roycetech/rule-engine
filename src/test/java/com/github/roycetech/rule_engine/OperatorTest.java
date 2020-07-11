package com.github.roycetech.rule_engine;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Test for Operator class.
 *
 * @author royce
 */
public class OperatorTest {

    @Test
    public final void testFromChar_and()
    {
	assertEquals(Operator.AND, Operator.fromChar('&'));
    }

    @Test
    public final void testFromChar_or()
    {
	assertEquals(Operator.OR, Operator.fromChar('|'));
    }

    @Test
    public final void testFromChar_not()
    {
	assertEquals(Operator.NOT, Operator.fromChar('!'));
    }

    @Test
    public final void testFromString_and()
    {
	assertEquals(Operator.AND, Operator.fromString("&"));
    }

    /**
     * Verify that exception is thrown for unknown symbol.
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testFromCharException()
    {
	Operator.fromChar('a');
    }

    /**
     * Verify that exception is thrown for unknown symbol.
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testFromString_exception()
    {
	Operator.fromString("yo");
    }

    @Test
    public final void testGetPrecedence_not()
    {
	assertTrue(Operator.NOT.getPrecedence() > Operator.AND.getPrecedence());
    }

    @Test
    public final void testGetPrecedence_or()
    {
	assertTrue(Operator.AND.getPrecedence() > Operator.OR.getPrecedence());
    }

    @Test
    public final void testGetSymbol()
    {
	assertEquals('!', Operator.NOT.getSymbol());
    }

    @Test
    public final void testToString_not()
    {
	assertEquals("Not", Operator.NOT.toWord());
    }

    @Test
    public final void testToString_and()
    {
	assertEquals("And", Operator.AND.toWord());
    }

    @Test
    public final void testToString_or()
    {
	assertEquals("Or", Operator.OR.toWord());
    }
}
