/**
 *
 */
package io.github.roycetech.ruleengine;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import io.github.roycetech.ruleengine.LogicHelper;
import io.github.roycetech.ruleengine.Operator;
import io.github.roycetech.ruleengine.Token;
import utils.PrivateMethodInvoker;

/**
 * Has reference to rast.
 *
 * @author royce
 */
public class LogicHelperTest {

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#performLogical(java.util.List, io.github.roycetech.ruleengine.Token[], io.github.roycetech.ruleengine.Operator)}.
	 */
	@Test
	public final void testPerformLogical_AND_bothInternalFalse()
	{
		final Token[] tokens = new Token[] { new Token(LogicHelper.IFALSE),
				new Token(LogicHelper.IFALSE) };
		assertFalse(Boolean.valueOf(LogicHelper.performLogical(null, tokens, Operator.AND)));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#performLogical(java.util.List, io.github.roycetech.ruleengine.Token[], io.github.roycetech.ruleengine.Operator)}.
	 *
	 * Symbiotic test.
	 */
	@Test
	public final void testPerformLogical_OR_false_false_unindexed()
	{
		final Token[] tokens = new Token[] { new Token(LogicHelper.IFALSE, -1),
				new Token(LogicHelper.IFALSE, -1) };
		assertEquals(LogicHelper.IFALSE, LogicHelper.performLogical(null, tokens, Operator.OR));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#performLogical(java.util.List, io.github.roycetech.ruleengine.Token[], io.github.roycetech.ruleengine.Operator)}.
	 *
	 * Symbiotic test of Recruiter.
	 */
	@Test
	public final void testPerformLogical_short_AND()
	{
		final Token[] tokens = new Token[] { new Token(LogicHelper.ITRUE, -1),
				new Token("Engineer", -1) };

		final List<Object> scenario = Arrays.asList("Engineer", 9);

		assertTrue(Boolean.valueOf(LogicHelper.performLogical(scenario, tokens, Operator.AND)));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#performLogical(java.util.List, io.github.roycetech.ruleengine.Token[], io.github.roycetech.ruleengine.Operator)}.
	 *
	 * Symbiotic test of Positive2.
	 */
	@Test
	public final void testPerformLogical_short_OR()
	{
		final Token[] tokens = new Token[] { new Token("0.5", -1),
				new Token(LogicHelper.IFALSE, -1) };

		final List<Object> scenario = Arrays.asList(-1);

		assertFalse(Boolean.valueOf(LogicHelper.performLogical(scenario, tokens, Operator.OR)));
	}

//    /**
//     * Test method for
//     * {@link com.github.roycetech.rule_engine.LogicHelper#performLogical(java.util.List, com.github.roycetech.rule_engine.Token[], com.github.roycetech.rule_engine.Operator)}.
//     */
//    @Test
//    public final void testPerformLogical_AND_bothInternalTrue()
//    {
//	final Token[] tokens = new Token[] {
//		new Token(LogicHelper.ITRUE),
//		new Token(LogicHelper.ITRUE)
//	};
//	final List<Object> scenario = Arrays.asList(true, true);
//	assertTrue(LogicHelper.performLogical(scenario, tokens, Operator.AND));
//    }

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isOpenBracket(char)}.
	 */
	@Test
	public final void testIsOpenBracket_positive()
	{
		assertTrue(LogicHelper.isOpenBracket('('));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isOpenBracket(char)}.
	 */
	@Test
	public final void testIsOpenBracket_negative()
	{
		assertFalse(LogicHelper.isOpenBracket('['));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isCloseBracket(char)}.
	 */
	@Test
	public final void testIsCloseBracket_positive()
	{
		assertTrue(LogicHelper.isCloseBracket(')'));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isCloseBracket(char)}.
	 */
	@Test
	public final void testIsCloseBracket_negative()
	{
		assertFalse(LogicHelper.isCloseBracket('}'));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isTrue(java.lang.String)}.
	 */
	@Test
	public final void testIsTrue_positive()
	{
		assertTrue(LogicHelper.isTrue("*true"));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isTrue(java.lang.String)}.
	 */
	@Test
	public final void testIsTrue_negative()
	{
		assertFalse(LogicHelper.isTrue("true"));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isFalse(java.lang.String)}.
	 */
	@Test
	public final void testIsFalse_positive()
	{
		assertTrue(LogicHelper.isFalse("*false"));

	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isInternal(java.lang.String)}.
	 */
	@Test
	public final void testIsInternal_true()
	{
		assertTrue(LogicHelper.isInternal("*true"));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isInternal(java.lang.String)}.
	 */
	@Test
	public final void testIsInternal_false()
	{
		assertTrue(LogicHelper.isInternal("*false"));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isInternal(java.lang.String)}.
	 */
	@Test
	public final void testIsInternal_no()
	{
		assertFalse(LogicHelper.isInternal("true"));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isOperator(java.lang.Object)}.
	 */
	@Test
	public final void testIsOperator_and()
	{
		assertTrue(LogicHelper.isOperator("&"));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isOperator(java.lang.Object)}.
	 */
	@Test
	public final void testIsOperator_negative_null()
	{
		assertFalse(LogicHelper.isOperator(null));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isOperator(java.lang.Object)}.
	 */
	@Test
	public final void testIsOperator_negative_empty()
	{
		assertFalse(LogicHelper.isOperator(""));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isOperator(java.lang.Object)}.
	 */
	@Test
	public final void testIsOperator_negative_tooLong()
	{
		assertFalse(LogicHelper.isOperator("&&"));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#isOperator(java.lang.Object)}.
	 */
	@Test
	public final void testIsOperator_negative_unknownSymbol()
	{
		assertFalse(LogicHelper.isOperator("*"));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#formatInternalResult(java.lang.String)}.
	 */
	@Test
	public final void testFormatInternalResult_true()
	{
		assertEquals("*true", LogicHelper.formatInternalResult("true"));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#formatInternalResult(java.lang.String)}.
	 */
	@Test
	public final void testFormatInternalResult_itrue()
	{
		assertEquals("*true", LogicHelper.formatInternalResult("*true"));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#formatInternalResult(java.lang.String)}.
	 */
	@Test
	public final void testFormatInternalResult_false()
	{
		assertEquals("*false", LogicHelper.formatInternalResult("false"));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#formatInternalResult(java.lang.String)}.
	 */
	@Test
	public final void testFormatInternalResult_ifalse()
	{
		assertEquals("*false", LogicHelper.formatInternalResult("*false"));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#evaluateOr(List,Token, Token)}.
	 */
	@Test
	public final void testEvaluateOr_short()
	{
		final PrivateMethodInvoker pmi = new PrivateMethodInvoker(LogicHelper.class, "evaluateOr",
				List.class, Token.class, Token.class);

		final Token left = new Token("apple");
		final Token right = new Token("orange");
		final List<Object> scenario = Arrays.asList(new Object[] { "apple" });

		final boolean actual = pmi.<Boolean>invoke(null, scenario, left, right);

		assertTrue(actual);
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#evaluateOr(List, Token, Token)}.
	 */
	@Test
	public final void testEvaluateOr_long()
	{
		final PrivateMethodInvoker pmi = new PrivateMethodInvoker(LogicHelper.class, "evaluateOr",
				List.class, Token.class, Token.class);

		final Token left = new Token("apple");
		final Token right = new Token("orange");
		final List<Object> scenario = Arrays.asList(new Object[] { "orange" });

		final boolean actual = pmi.<Boolean>invoke(null, scenario, left, right);

		assertTrue(actual);
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#evaluateAnd(List, Token, Token)}.
	 */
	@Test
	public final void testEvaluateAnd_short()
	{
		final PrivateMethodInvoker pmi = new PrivateMethodInvoker(LogicHelper.class, "evaluateAnd",
				List.class, Token.class, Token.class);

		final Token left = new Token("apple");
		final Token right = new Token("orange");
		final List<Object> scenario = Arrays.asList(new Object[] { "orange" });

		final boolean actual = pmi.<Boolean>invoke(null, scenario, left, right);

		assertFalse(actual);
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#evaluateAnd(java.util.List, Token, Token)}.
	 */
	@Test
	public final void testEvaluateAnd_long()
	{
		final PrivateMethodInvoker pmi = new PrivateMethodInvoker(LogicHelper.class, "evaluateAnd",
				List.class, Token.class, Token.class);

		final Token left = new Token("apple");
		final Token right = new Token("orange");
		final List<Object> scenario = Arrays.asList(new Object[] { "banana" });

		final boolean actual = pmi.<Boolean>invoke(null, scenario, left, right);

		assertFalse(actual);
	}

	// TEST Private methods
	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.LogicHelper#performBothInternal(Token, Token, Operator)}.
	 *
	 * Symbiotic test, HotelFinder.
	 */
	@Test
	public final void testPerformBothInternal()
	{
		final Token left = new Token(LogicHelper.IFALSE, -1);
		final Token right = new Token(LogicHelper.ITRUE, -1);

		final PrivateMethodInvoker pmi = new PrivateMethodInvoker(LogicHelper.class,
				"performBothInternal", Token.class, Token.class, Operator.class);

		final String actual = pmi.<String>invoke(null, left, right, Operator.OR);

		assertEquals(LogicHelper.ITRUE, actual);

	}

}
