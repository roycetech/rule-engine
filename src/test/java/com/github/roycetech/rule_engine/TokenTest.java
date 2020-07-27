package com.github.roycetech.rule_engine;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.github.roycetech.converter.StrConverter;

/**
 * Test class for {@link com.github.roycetech.rule_engine.Token}.
 */
public class TokenTest {

    private static final String ORANGE = "orange";
    private static final String APPLE = "apple";

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.Token#Token(String)}.
     */
    @Test
    public final void testConstructorString_withSubscript()
    {
	assertEquals("token", new Token("token[0]").getValue());
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.Token#Token(String)}.
     */
    @Test
    public final void testConstructorString_noSubscript()
    {
	assertEquals("token", new Token("token").getValue());
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.Token#isNegative()}.
     */
    @Test
    public final void testIsNegative()
    {
	// @formatter:off
	final Object[][] testData = {
	    { new Token(null, -1), true },
	    { new Token(null, 0), false },
	    { new Token(null, 1), false }
	};
	// @formatter:on

	for (final Object[] objects : testData) {
	    final Token sut = (Token) objects[0];
	    final boolean expected = (boolean) objects[1];
	    assertEquals(expected, sut.isNegative());
	}
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.Token#equalsInternal(String)}.
     */
    @Test
    public final void testEqualsInternal()
    {
	// @formatter:off
	final Object[][] testData = {
	    { new Token("1", -1),               LogicHelper.IFALSE, false },
	    { new Token(LogicHelper.ITRUE, -1),  LogicHelper.IFALSE, false },
	    { new Token(LogicHelper.IFALSE, -1), LogicHelper.IFALSE, true },

	    { new Token("2", 0),                LogicHelper.IFALSE, false },
	    { new Token(LogicHelper.ITRUE, 0),   LogicHelper.IFALSE, false },
	    { new Token(LogicHelper.IFALSE, 0),  LogicHelper.IFALSE, false },

	    { new Token("3", -1),               LogicHelper.ITRUE, false },
	    { new Token(LogicHelper.ITRUE, -1),  LogicHelper.ITRUE, true },
	    { new Token(LogicHelper.IFALSE, -1), LogicHelper.ITRUE, false },

	    { new Token("4", 0),                LogicHelper.ITRUE, false },
	    { new Token(LogicHelper.ITRUE, 0),   LogicHelper.ITRUE, false },
	    { new Token(LogicHelper.IFALSE, 0),  LogicHelper.ITRUE, false }
	};
	// @formatter:on

	for (final Object[] objects : testData) {
	    final Token sut = (Token) objects[0];
	    final String internal = (String) objects[1];

	    final boolean expected = (boolean) objects[2];
	    assertEquals(expected, sut.equalsInternal(internal));
	}
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.Token#accepts(List)}.
     */
    @Test
    public final void testAccepts()
    {
	// @formatter:off
	final Object[][] testData =  {
	    { new Token(APPLE, -1),  new Object[] { APPLE }, true },
	    { new Token(ORANGE, -1), new Object[] { APPLE }, false },
	    { new Token(APPLE, 0),   new Object[] { APPLE, ORANGE }, true },
	    { new Token("guava", 0),   new Object[] { APPLE }, false },
	    { new Token(ORANGE, 0),   new Object[] { APPLE, ORANGE }, false },
	};
	// @formatter:on

	for (final Object[] objects : testData) {
	    final Token sut = (Token) objects[0];
	    final List<Object> scenario = Arrays.asList((Object[]) objects[1]);

	    final boolean expected = (boolean) objects[2];
	    assertEquals(expected, sut.accepts(scenario));
	}
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.Token#accepts(List)}.
     */
    @Test
    public final void testAccepts_converter()
    {
	// @formatter:off
	final Object[][] testData =  {
		{ new Token(APPLE, -1),  new Object[] { APPLE }, true },
		{ new Token(ORANGE, -1), new Object[] { APPLE }, false },
		{ new Token(APPLE, 0),   new Object[] { APPLE, ORANGE }, true },
		{ new Token("guava", 0),   new Object[] { APPLE }, false },
		{ new Token(ORANGE, 0),   new Object[] { APPLE, ORANGE }, false },
	};
	// @formatter:on

	final StrConverter converter = new StrConverter();
	for (final Object[] objects : testData) {
	    final Token sut = (Token) objects[0];
	    final List<Object> scenario = Arrays.asList((Object[]) objects[1]);

	    final boolean expected = (boolean) objects[2];
	    assertEquals(expected, sut.accepts(scenario, converter));
	}
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.Token#toString()}.
     */
    @Test
    public final void testToString()
    {
	assertEquals("apple[-1]", new Token(APPLE, -1).toString());
    }
}
