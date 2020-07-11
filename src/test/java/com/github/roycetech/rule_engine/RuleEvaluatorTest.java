/**
 *
 */
package com.github.roycetech.rule_engine;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import com.github.roycetech.converter.BoolConverter;
import com.github.roycetech.converter.ElementConverter;

/**
 * @author royce
 *
 */
public class RuleEvaluatorTest {

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.RuleEvaluator#parse(java.lang.Object)}.
     */
    @Test
    public final void testParse_string()
    {
	final Map<String, ElementConverter> tokenConverter = new HashMap<>();
	tokenConverter.put("false", new BoolConverter());
	tokenConverter.put("true", new BoolConverter());

	final RuleEvaluator sut = new RuleEvaluator(tokenConverter);
	sut.parse("true[0]&true[1]");

	assertArrayEquals(new String[] { "&", "true[1]", "true[0]"
	}, sut.getStackRPN().toArray(new String[0]));
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.RuleEvaluator#parse(java.lang.Object)}.
     *
     * Symbiotic, EnumModule.
     */
    @Test
    public final void testParse_array()
    {
	final Map<String, ElementConverter> tokenConverter = new HashMap<>();

	final RuleEvaluator sut = new RuleEvaluator(tokenConverter);
	sut.parse(new Object[] { new Object[] { "- a", "- b"
		}, "|", new Object[] { "These are the options:", "- a", "- b"
		}
	});

	assertArrayEquals(new Object[] {
		"|",
		new Object[] { "These are the options:", "- a", "- b"
		},
		new Object[] { "- a", "- b"
		}
	}, sut.getStackRPN().toArray(new Object[0]));
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.RuleEvaluator#evaluate(java.util.List)}.
     */
    @Test
    public final void testEvaluate_single()
    {
	final DequeEvaluator mockDequeEvaluator = mock(DequeEvaluator.class);
	when(mockDequeEvaluator.evaluateOneRpn(null)).thenReturn(true);
	when(mockDequeEvaluator.evaluateMultiRpn(null)).thenReturn(false);

	final RuleEvaluator sut = new RuleEvaluator(null);
	sut.setDequeEvaluator(mockDequeEvaluator);
	final Deque<Object> single = new ArrayDeque<>();
	single.add("one");
	sut.setStackRPN(single);
	assertTrue(sut.evaluate(null));
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.RuleEvaluator#evaluate(java.util.List)}.
     */
    @Test
    public final void testEvaluate_multi()
    {
	final DequeEvaluator mockDequeEvaluator = mock(DequeEvaluator.class);
	when(mockDequeEvaluator.evaluateOneRpn(null)).thenReturn(false);
	when(mockDequeEvaluator.evaluateMultiRpn(null)).thenReturn(true);

	final RuleEvaluator sut = new RuleEvaluator(null);
	sut.setDequeEvaluator(mockDequeEvaluator);
	final Deque<Object> multi = new ArrayDeque<>();
	multi.add("one");
	multi.add("two");
	sut.setStackRPN(multi);
	assertTrue(sut.evaluate(null));
    }
}
