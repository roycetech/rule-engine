/**
 *
 */
package com.github.roycetech.rule_engine;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.spy;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.github.roycetech.converter.BoolConverter;
import com.github.roycetech.converter.ElementConverter;
import com.github.roycetech.converter.IntConverter;
import com.github.roycetech.converter.StrConverter;

import utils.PrivateMethodInvoker;

/**
 * Test here derived from already working test scenarios of rast. So the way
 * some tests are set up may not make sense because of that.
 *
 * @author royce
 */
public class DequeEvaluatorTest {

//    private static final String ORANGE = "orange";
    private static final String APPLE = "apple";

    /**
     * Ignorable instances.
     */
    private static final Deque<Object> DUMMY_DEQUE = new ArrayDeque<>();
    private static final Deque<Object> DUMMY_DEQUE_OBJ = new ArrayDeque<>();
    private static final List<Object> DUMMY_SCENARIO = new ArrayList<>();
    private static final HashMap<String, ElementConverter> DUMMY_CONVERTERS = new HashMap<>();

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.DequeEvaluator#evaluateOneRpn(java.util.List)}.
     */
    @Test
    public final void testEvaluateOneRpn_coverage()
    {
	final Deque<Object> stackRPN = new ArrayDeque<>();
	stackRPN.push(APPLE);
	final DequeEvaluator subject = new DequeEvaluator(stackRPN, null, null);
	assertTrue(subject.evaluateOneRpn(Arrays.asList(APPLE)));
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.DequeEvaluator#evaluateMultiRpn(java.util.List)}.
     */
    @Test
    public final void testEvaluateMultiRpn()
    {
	final Map<String, ElementConverter> tokenConverters = new HashMap<>();
	tokenConverters.put("false", new BoolConverter());
	tokenConverters.put("true", new BoolConverter());

	final Deque<Object> stackRPN = new ArrayDeque<>();
	stackRPN.add(String.valueOf(Operator.AND.getSymbol()));
	stackRPN.add("true[1]");
	stackRPN.add("true[0]");

	final DequeEvaluator subject = new DequeEvaluator(stackRPN,
		new ArrayDeque<>(), tokenConverters);

	assertFalse(subject.evaluateMultiRpn(Arrays.asList(false, false)));
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.DequeEvaluator#evaluateMultiRpn(java.util.List)}.
     */
    @Test(expected = RuleEvaluatorException.class)
    public final void testEvaluateMultiRpn_bad()
    {

	final Deque<Object> stackAnswer = new ArrayDeque<>();
	stackAnswer.add("1");
	stackAnswer.add("2");

	final DequeEvaluator subject = new DequeEvaluator(DUMMY_DEQUE,
		DUMMY_DEQUE_OBJ, DUMMY_CONVERTERS);
	final DequeEvaluator spySubject = spy(subject);

	final List<Object> scenario = new ArrayList<>();

	Mockito.doAnswer(new Answer<Object>() {

	    @Override
	    public Object answer(final InvocationOnMock invocation)
		    throws Throwable
	    {
		spySubject.setStackAnswer(stackAnswer);
		return null;
	    }
	}).when(spySubject).evaluateStackRpn(Mockito.<Deque<Object>>any(),
		Mockito.<List<Object>>any());

	spySubject.evaluateMultiRpn(scenario);
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.DequeEvaluator#evaluateOperator(java.util.List, java.lang.String)}.
     *
     * Coverage test.
     */
    @Test
    public final void testEvaluateOperator_not()

    {
	final DequeEvaluator subject = new DequeEvaluator(DUMMY_DEQUE,
		DUMMY_DEQUE_OBJ, DUMMY_CONVERTERS);
	final DequeEvaluator spySubject = spy(subject);

	Mockito.doNothing().when(spySubject)
		.evaluateMultiNot(Mockito.<List<Object>>any());

	final PrivateMethodInvoker pmi = new PrivateMethodInvoker(
		DequeEvaluator.class, "evaluateOperator", List.class,
		Character.TYPE);
	pmi.invoke(spySubject, DUMMY_SCENARIO, '!');

	assertTrue(true);
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.DequeEvaluator#evaluateMultiNot(java.util.List)}.
     */
    @Test
    public final void testEvaluateMultiNot_internalFalse()
    {
	final Deque<Object> stackAnswers = new ArrayDeque<>();
	stackAnswers.add("*true");

	final DequeEvaluator subject = new DequeEvaluator(DUMMY_DEQUE,
		stackAnswers, DUMMY_CONVERTERS);

	final PrivateMethodInvoker pmi = new PrivateMethodInvoker(
		DequeEvaluator.class, "evaluateMultiNot", List.class);
	pmi.invoke(subject, DUMMY_SCENARIO);

	assertEquals("*false", subject.getStackAnswer().peekLast());
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.DequeEvaluator#evaluateMultiNot(java.util.List)}.
     */
    @Test
    public final void testEvaluateMultiNot_internalTrue()
	    throws NoSuchMethodException, SecurityException,
	    IllegalAccessException, IllegalArgumentException,
	    InvocationTargetException
    {
	final Deque<Object> stackAnswers = new ArrayDeque<>();
	stackAnswers.add("*false");

	final DequeEvaluator subject = new DequeEvaluator(DUMMY_DEQUE,
		stackAnswers, DUMMY_CONVERTERS);

	final PrivateMethodInvoker pmi = new PrivateMethodInvoker(
		DequeEvaluator.class, "evaluateMultiNot", List.class);
	pmi.invoke(subject, DUMMY_SCENARIO);

	assertEquals("*true", subject.getStackAnswer().peekLast());
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.DequeEvaluator#evaluateMultiNot(java.util.List)}.
     */
    @Test
    public final void testEvaluateMultiNot_nonInternal()
    {
	final Map<String, ElementConverter> tokenConverters = new HashMap<>();
	tokenConverters.put("false", new BoolConverter());
	tokenConverters.put("true", new BoolConverter());
	tokenConverters.put("basic", new StrConverter());

	final Deque<Object> stackAnswers = new ArrayDeque<>();
	stackAnswers.add("*true");
	stackAnswers.add("false[1]");
	stackAnswers.add("basic");

	final DequeEvaluator subject = new DequeEvaluator(null, stackAnswers,
		tokenConverters);

	final List<Object> scenario = Arrays.asList(false, false, false,
		"basic");

	final PrivateMethodInvoker pmi = new PrivateMethodInvoker(
		DequeEvaluator.class, "evaluateMultiNot", List.class);
	pmi.invoke(subject, scenario);
	assertEquals("*false", subject.getStackAnswer().peekLast());
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.DequeEvaluator#evaluateNonInternal(java.util.List, java.lang.String)}.
     *
     * Coverage test.
     */
    @Test
    public final void testEvaluateNonInternal()
    {
	final DequeEvaluator subject = new DequeEvaluator(DUMMY_DEQUE,
		DUMMY_DEQUE_OBJ, DUMMY_CONVERTERS);

	final PrivateMethodInvoker pmi = new PrivateMethodInvoker(
		DequeEvaluator.class, "evaluateNonInternal", List.class,
		String.class);

	final String actual = pmi.<String>invoke(subject,
		Arrays.asList("right"), "wrong");
	assertTrue(Boolean.valueOf(actual));
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.DequeEvaluator#evaluateMulti(java.util.List, com.github.roycetech.rule_engine.Operator)}.
     *
     * Symbiotic test, Positive2.
     */
    @Test
    public final void testEvaluateMulti()
    {
	final Deque<Object> stackAnswers = new ArrayDeque<>();
	stackAnswers.add("1");
	stackAnswers.add("999");

	final Map<String, ElementConverter> tokenConverters = new HashMap<>();
	tokenConverters.put("1", new IntConverter());
	tokenConverters.put("999", new IntConverter());

	final DequeEvaluator subject = new DequeEvaluator(DUMMY_DEQUE,
		stackAnswers, tokenConverters);

	final List<Object> scenario = Arrays.asList("null");

	final PrivateMethodInvoker pmi = new PrivateMethodInvoker(
		DequeEvaluator.class, "evaluateMulti", List.class,
		Operator.class);

	pmi.invoke(subject, scenario, Operator.OR);
	assertEquals("*false", subject.getStackAnswer().peekLast());
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.DequeEvaluator#nextValue()}.
     */
    @Test
    public final void testNextValue_array()
    {
	final DequeEvaluator subject = new DequeEvaluator(DUMMY_DEQUE,
		DUMMY_DEQUE_OBJ, DUMMY_CONVERTERS);

	final Deque<Object> stackAnswers = new ArrayDeque<>();
	stackAnswers.add("first");

	final Object[] testArray = new Object[] { 1, 2
	};
	stackAnswers.add(testArray);

	subject.setStackAnswer(stackAnswers);

	final PrivateMethodInvoker pmi = new PrivateMethodInvoker(
		DequeEvaluator.class, "nextValue");
	final Token token = pmi.<Token>invoke(subject);

	assertEquals(testArray, token.getValue());
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.DequeEvaluator#nextValue()}.
     */
    @Test
    public final void testNextValue_internal()
    {
	final DequeEvaluator subject = new DequeEvaluator(DUMMY_DEQUE,
		DUMMY_DEQUE_OBJ, DUMMY_CONVERTERS);

	final Deque<Object> stackAnswers = new ArrayDeque<>();
	stackAnswers.add("first");

	stackAnswers.add(LogicHelper.ITRUE);

	subject.setStackAnswer(stackAnswers);

	final PrivateMethodInvoker pmi = new PrivateMethodInvoker(
		DequeEvaluator.class, "nextValue");
	final Token token = pmi.<Token>invoke(subject);

	assertEquals(LogicHelper.ITRUE, token.getValue());
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.DequeEvaluator#nextValueDefault(java.lang.String)}.
     */
    @Test(expected = RuleEvaluatorException.class)
    public final void testNextValueDefault_exception()
    {
	final DequeEvaluator subject = new DequeEvaluator(DUMMY_DEQUE,
		DUMMY_DEQUE_OBJ, DUMMY_CONVERTERS);

	final PrivateMethodInvoker pmi = new PrivateMethodInvoker(
		DequeEvaluator.class, "nextValueDefault", String.class);
	pmi.invoke(subject, "test");
    }

}
