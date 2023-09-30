/**
 *
 */
package io.github.roycetech.ruleengine.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;

import io.github.roycetech.ruleengine.RuleEngineException;
import io.github.roycetech.ruleengine.utils.ReflectionHelper;

/**
 * @author royce
 *
 */
public class ReflectionHelperTest {

	/**
	 * Test method for {@link ReflectionHelper#ReflectionHelper(Class<?>, String,
	 * Class<?>[])}.
	 */
	@Test
	public final void testReflectionHelper_happy()
	{
		assertNotNull(new ReflectionHelper(Dummy.class, "method"));
	}

	/**
	 * Test method for {@link ReflectionHelper#ReflectionHelper(Class<?>, String,
	 * Class<?>[])}.
	 */
	@Test(expected = RuleEngineException.class)
	public final void testReflectionHelper_invalidMethod()
	{
		new ReflectionHelper(Dummy.class, "methodXXX");
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.utils.ReflectionHelper#invoke(java.lang.Object, java.lang.Object[])}.
	 */
	@Test
	public final void testInvoke_happy()
	{
		final ReflectionHelper sut = new ReflectionHelper(Dummy.class, "method");
		final Dummy dummy = new Dummy();
		assertEquals("test", sut.invoke(dummy));
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.utils.ReflectionHelper#invoke(java.lang.Object, java.lang.Object[])}.
	 */
	@Test(expected = RuleEngineException.class)
	public final void testInvoke_badAccess()
	{
		final ReflectionHelper sut = new ReflectionHelper(Dummy.class, "secret");
		final Dummy dummy = new Dummy();
		sut.invoke(dummy);
	}

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.utils.ReflectionHelper#invoke(java.lang.Object, java.lang.Object[])}.
	 */
	@Test(expected = RuleEngineException.class)
	public final void testInvoke_broken()
	{
		final ReflectionHelper sut = new ReflectionHelper(Dummy.class, "broken");
		final Dummy dummy = new Dummy();
		sut.invoke(dummy);
	}
}

class Dummy {
	String method()
	{
		return "test";
	}

	@SuppressWarnings("unused")
	private String secret()
	{
		return "sauce";
	}

	@SuppressWarnings("unused")
	void broken()
	{
		throw new RuleEngineException("Test");
	}

}