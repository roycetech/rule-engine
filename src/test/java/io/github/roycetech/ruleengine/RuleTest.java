/**
 *
 */
package io.github.roycetech.ruleengine;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import io.github.roycetech.ruleengine.Rule;

/**
 * @author royce
 *
 */
public class RuleTest {

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Rule#Rule(java.util.Map)}.
	 */
	@Test(expected = IllegalArgumentException.class)
	public final void testRule_null()
	{
		new Rule(null);
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Rule#Rule(java.util.Map)}.
	 */
	@Test(expected = IllegalArgumentException.class)
	public final void testRule_empty()
	{
		new Rule(new HashMap<String, Object>());
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Rule#sanitize(java.lang.Object)}.
	 */
	@Test
	public final void testSanitize_string()
	{
		assertEquals("(true&false|false&!true)",
				Rule.sanitize("  ( true &   false | false  & !true) "));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Rule#sanitize(java.lang.Object)}.
	 */
	@Test
	public final void testSanitize_Array()
	{
	// @formatter:off
	final Integer[] sampleArray = { 1, 2 };
	// @formatter:on

		assertEquals(sampleArray, Rule.sanitize(sampleArray));
	}

	/**
	 * Test method for {@link io.github.roycetech.ruleengine.Rule#getSize()}.
	 */
	@Test
	public final void testGetSize()
	{
		final Map<String, Object> rules = new HashMap<>();
		rules.put("1", "2");
		final Rule sut = new Rule(rules);

		assertEquals(1, sut.getSize());
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.Rule#getClause(java.lang.String)}.
	 */
	@Test
	public final void testGetClause()
	{
		final Map<String, Object> rules = new HashMap<>();
		rules.put("1", "2");
		final Rule sut = new Rule(rules);

		assertEquals("2", sut.getClause("1"));
	}

}
