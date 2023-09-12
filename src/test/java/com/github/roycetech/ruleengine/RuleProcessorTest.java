/**
 *
 */
package com.github.roycetech.ruleengine;

import static org.junit.Assert.assertArrayEquals;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import com.github.roycetech.ruleengine.converter.BooleanConverter;
import com.github.roycetech.ruleengine.converter.ElementConverter;

/**
 * @author royce
 *
 */
public class RuleProcessorTest {

//    /**
//     * Test method for {@link com.github.roycetech.rule_engine.RuleProcessor#RuleProcessor(com.github.roycetech.rule_engine.Rule, java.util.Map)}.
//     */
//    @Test
//    public final void testRuleProcessor()
//    {
//	fail("Not yet implemented");
//    }

	/**
	 * Test method for
	 * {@link com.github.roycetech.ruleengine.RuleProcessor#evaluate(java.util.List)}.
	 */
	@Test
	public final void testEvaluate_false()
	{
		final Map<String, Object> outcomeClause = new HashMap<>();
		outcomeClause.put("true", "true[0] & true[1]");

		final Rule rule = new Rule(outcomeClause);
		final Map<String, ElementConverter> tokenConverters = new HashMap<>();
		tokenConverters.put("false", new BooleanConverter());
		tokenConverters.put("true", new BooleanConverter());

		final List<Object> scenario = Arrays.asList(new Object[] { true, false });

		final RuleProcessor sut = new RuleProcessor(rule, tokenConverters);
		assertArrayEquals(new Boolean[] { Boolean.FALSE }, sut.evaluate(scenario));

	}

	/**
	 * Test method for
	 * {@link com.github.roycetech.ruleengine.RuleProcessor#evaluate(java.util.List)}.
	 */
	@Test
	public final void testEvaluate_true()
	{
		final Map<String, Object> outcomeClause = new HashMap<>();
		outcomeClause.put("true", "true[0] & true[1]");

		final Rule rule = new Rule(outcomeClause);
		final Map<String, ElementConverter> tokenConverters = new HashMap<>();
		tokenConverters.put("false", new BooleanConverter());
		tokenConverters.put("true", new BooleanConverter());

		final List<Object> scenario = Arrays.asList(new Object[] { true, true });

		final RuleProcessor sut = new RuleProcessor(rule, tokenConverters);
		assertArrayEquals(new Boolean[] { Boolean.TRUE }, sut.evaluate(scenario));

	}
}
