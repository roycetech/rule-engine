package io.github.roycetech.ruleengine;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import io.github.roycetech.ruleengine.Operator;

/**
 * Test for Operator class.
 *
 * @author royce
 */
public class OperatorTest {

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Operator#fromChar(char)}.
	 */
	@Test
	public final void testFromChar_and()
	{
		assertEquals(Operator.AND, Operator.fromChar('&'));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Operator#fromChar(char)}.
	 */
	@Test
	public final void testFromChar_or()
	{
		assertEquals(Operator.OR, Operator.fromChar('|'));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Operator#fromChar(char)}.
	 */
	@Test
	public final void testFromChar_not()
	{
		assertEquals(Operator.NOT, Operator.fromChar('!'));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Operator#fromString(java.lang.String)}.
	 */
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

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Operator#getPrecedence()}.
	 */
	@Test
	public final void testGetPrecedence_not()
	{
		assertTrue(Operator.NOT.getPrecedence() > Operator.AND.getPrecedence());
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Operator#getPrecedence()}.
	 */
	@Test
	public final void testGetPrecedence_or()
	{
		assertTrue(Operator.AND.getPrecedence() > Operator.OR.getPrecedence());
	}

	/**
	 * Test method for {@link io.github.roycetech.ruleengine.Operator#getSymbol()}.
	 */
	@Test
	public final void testGetSymbol()
	{
		assertEquals('!', Operator.NOT.getSymbol());
	}

	/**
	 * Test method for {@link io.github.roycetech.ruleengine.Operator#toString()}.
	 */
	@Test
	public final void testToString_not()
	{
		assertEquals("Not", Operator.NOT.toWord());
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Operator#getPrecedence()}.
	 */
	@Test
	public final void testToString_and()
	{
		assertEquals("And", Operator.AND.toWord());
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Operator#getPrecedence()}.
	 */
	@Test
	public final void testToString_or()
	{
		assertEquals("Or", Operator.OR.toWord());
	}
}
