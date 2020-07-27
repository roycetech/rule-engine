/**
 *
 */
package com.github.roycetech.rule_engine.utils;

import static org.junit.Assert.assertArrayEquals;

import java.util.ArrayDeque;
import java.util.Deque;

import org.junit.Test;

import utils.PrivateMethodInvoker;

/**
 * Testing a binary operation true[0]&true[1]
 *
 * @author royce
 */
public class ShunterTest {

    /**
     * Ignorable instances.
     */
    private static final Deque<Object> DUMMY_DEQUE = new ArrayDeque<>();

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.utils.Shunter#shuntInternal(java.lang.Object)}.
     */
    @Test
    public final void testShuntInternal_Token1()
    {
	final Shunter sut = new Shunter(new ArrayDeque<>(), new ArrayDeque<>());
	sut.shuntInternal("true[0]");
	assertArrayEquals(new String[] { "true[0]"
	}, sut.getStackRPN().toArray(new String[0]));
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.utils.Shunter#shuntInternal(java.lang.Object)}.
     */
    @Test
    public final void testShuntInternal_Token2()
    {
	final Shunter sut = new Shunter(new ArrayDeque<>(), new ArrayDeque<>());
	sut.shuntInternal("true[0]");
	sut.shuntInternal("&");

	assertArrayEquals(new String[] { "true[0]"
	}, sut.getStackRPN().toArray(new String[0]));
	assertArrayEquals(new String[] { "&"
	}, sut.getStackOperations().toArray(new String[0]));
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.utils.Shunter#shuntInternal(java.lang.Object)}.
     *
     * Symbiotic to rast LogicChecker. Result is slightly different in order,
     * because in Java, the collection object used is already in the reversed
     * order.
     */
    @Test
    public final void testShuntInternal_Token3()
    {
	final Shunter sut = new Shunter(new ArrayDeque<>(), new ArrayDeque<>());
	sut.shuntInternal("true[0]");
	sut.shuntInternal("&");
	sut.shuntInternal("true[1]");
	assertArrayEquals(new String[] { "true[1]", "true[0]"
	}, sut.getStackRPN().toArray(new String[0]));
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.utils.Shunter#shuntInternal(java.lang.Object)}.
     *
     * Symbiotic Recruiter.
     */
    @Test
    public final void testShuntInternal_openBracket()
    {
	final Shunter sut = new Shunter(DUMMY_DEQUE, DUMMY_DEQUE);
	sut.shuntInternal("(");
	assertArrayEquals(new String[] { "("
	}, sut.getStackOperations().toArray(new String[0]));
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.utils.Shunter#shuntInternal(java.lang.Object)}.
     *
     * Symbiotic Recruiter.
     */
    @Test
    public final void testShuntInternal_closeBracket()
    {
	final Deque<Object> stackOperations = new ArrayDeque<>();
	stackOperations.add("&");
	stackOperations.add("(");
	stackOperations.add("|");

	final Deque<Object> stackRpn = new ArrayDeque<>();
	stackRpn.add("Engineer");
	stackRpn.add("9");
	stackRpn.add("10");

	final Shunter sut = new Shunter(stackOperations, stackRpn);
	sut.shuntInternal(")");

	assertArrayEquals(new String[] { "&"
	}, sut.getStackOperations().toArray(new String[0]));

	assertArrayEquals(new String[] { "Engineer", "9", "10", "|"
	}, sut.getStackRPN().toArray(new String[0]));
    }

    // Testing Private Methods ===================================
    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.utils.Shunter#shuntClose()}.
     *
     * Symbiotic. Based off working example Recruiter from rast.
     */
    @Test
    public final void testShuntClose()
    {
	final Deque<Object> stackOperations = new ArrayDeque<>();
	stackOperations.add("&");
	stackOperations.add("(");
	stackOperations.add("|");

	final Deque<Object> stackRpn = new ArrayDeque<>();
	stackRpn.add("Engineer");
	stackRpn.add("9");
	stackRpn.add("10");

	final Shunter sut = new Shunter(stackOperations, stackRpn);

	final PrivateMethodInvoker pmi = new PrivateMethodInvoker(Shunter.class,
		"shuntClose");
	pmi.<Boolean>invoke(sut);

	assertArrayEquals(new String[] { "&"
	}, sut.getStackOperations().toArray(new String[0]));

	assertArrayEquals(new String[] { "Engineer", "9", "10", "|"
	}, sut.getStackRPN().toArray(new String[0]));
    }

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.utils.Shunter#shuntOperator(java.lang.String)}.
     *
     * Based off working example from rast.
     */
    @Test
    public final void testShuntOperator()
    {
	final Deque<Object> stackOperations = new ArrayDeque<>();
	stackOperations.add("&");

	final Deque<Object> stackRpn = new ArrayDeque<>();
	stackRpn.add("Manager");
	stackRpn.add("10");

	final Shunter sut = new Shunter(stackOperations, stackRpn);

	final PrivateMethodInvoker pmi = new PrivateMethodInvoker(Shunter.class,
		"shuntOperator", Object.class);
	pmi.<Boolean>invoke(sut, "|");

	assertArrayEquals(new String[] { "|"
	}, sut.getStackOperations().toArray(new String[0]));

	assertArrayEquals(new String[] { "Manager", "10", "&"
	}, sut.getStackRPN().toArray(new String[0]));
    }

}
