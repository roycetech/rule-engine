package com.github.roycetech.rule_engine;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

public class TokenTest {

    @Test
    public final void testIsNegative()
    {
	// @formatter:off
	final Object[][] testData = new Object[][] {
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

    @Test
    public final void testEqualsInternal()
    {
	// @formatter:off
	final Object[][] testData = new Object[][] {
	    { new Token("1", -1),               LogicHelper.FALSE, false },
	    { new Token(LogicHelper.TRUE, -1),  LogicHelper.FALSE, false },
	    { new Token(LogicHelper.FALSE, -1), LogicHelper.FALSE, true },

	    { new Token("2", 0),                LogicHelper.FALSE, false },
	    { new Token(LogicHelper.TRUE, 0),   LogicHelper.FALSE, false },
	    { new Token(LogicHelper.FALSE, 0),  LogicHelper.FALSE, false },

	    { new Token("3", -1),               LogicHelper.TRUE, false },
	    { new Token(LogicHelper.TRUE, -1),  LogicHelper.TRUE, true },
	    { new Token(LogicHelper.FALSE, -1), LogicHelper.TRUE, false },

	    { new Token("4", 0),                LogicHelper.TRUE, false },
	    { new Token(LogicHelper.TRUE, 0),   LogicHelper.TRUE, false },
	    { new Token(LogicHelper.FALSE, 0),  LogicHelper.TRUE, false }
	};
	// @formatter:on

	for (final Object[] objects : testData) {
	    final Token sut = (Token) objects[0];
	    final String internal = (String) objects[1];

	    final boolean expected = (boolean) objects[2];
	    assertEquals(expected, sut.equalsInternal(internal));
	}
    }

    @Test
    public final void testAccepts()
    {
	// @formatter:off
	final Object[][] testData = new Object[][] {
	    { new Token("apple", -1),  new Object[] { "apple" }, true },
	    { new Token("orange", -1), new Object[] { "apple" }, false },
	    { new Token("apple", 0),   new Object[] { "apple", "orange" }, true },
	    { new Token("guava", 0),   new Object[] { "apple" }, false },
	    { new Token("orange", 0),   new Object[] { "apple", "orange" }, false },
	};
	// @formatter:on

	for (final Object[] objects : testData) {
	    final Token sut = (Token) objects[0];
	    final List<Object> scenario = Arrays.asList((Object[]) objects[1]);

	    final boolean expected = (boolean) objects[2];
	    assertEquals(expected, sut.accepts(scenario));
	}
    }

    @Test
    public final void testToString()
    {
	assertEquals("apple[-1]", new Token("apple", -1).toString());
    }
}
