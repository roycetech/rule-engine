/**
 *
 */
package com.github.roycetech.rule_engine.utils;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import com.github.roycetech.rule_engine.RuleEngineException;

/**
 * The `ReflectionHelper` class is a utility class that simplifies the handling
 * of checked exceptions when making a reflection call to invoke a method. It
 * allows you to invoke methods on a class, even those with inaccessible or
 * checked exceptions, without the need to catch and handle those exceptions
 * manually.
 *
 * <p>
 * This class provides a clean and convenient way to perform reflection-based
 * method invocations, allowing you to access and call methods dynamically.
 *
 * <p>
 * Usage of this class can help abstract away the complexities of working with
 * reflection, especially when dealing with methods that may throw checked
 * exceptions.
 */
public class ReflectionHelper {

	/**
	 * Target method to invoke.
	 */
	private final Method method;

	/**
	 * Creates a new `ReflectionHelper` instance for invoking a specific method on a
	 * class.
	 *
	 * @param klass          The class that contains the method definition.
	 * @param methodName     The name of the method to be invoked.
	 * @param parameterTypes The parameter types of the method.
	 */
	public ReflectionHelper(final Class<?> klass, final String methodName,
			final Class<?>... parameterTypes) {
		try {
			this.method = klass.getDeclaredMethod(methodName, parameterTypes);
		} catch (NoSuchMethodException | SecurityException e) {
			throw new RuleEngineException(e);
		}
	}

	/**
	 * Invokes the method with the arguments and returns the result or throws the
	 * exception encountered during invocation.
	 *
	 * @param <T>    the type to use for casting the result.
	 *
	 * @param target where the method is to be invoked from, null for static
	 *               invocation.
	 * @param args   arguments used for the invocation.
	 * @return the result of the invocation.
	 */
	@SuppressWarnings("unchecked")
	public <T> T invoke(final Object target, final Object... args)
	{
		try {
			return (T) this.method.invoke(target, args);
		} catch (IllegalAccessException | IllegalArgumentException e) {
			throw new RuleEngineException(e);
		} catch (final InvocationTargetException e) {
			throw (RuntimeException) e.getTargetException();
		}
	}

	/**
	 * Forces access to otherwise inaccessible methods by making them accessible.
	 */
	public void forceAccess()
	{
		this.method.setAccessible(true);
	}
}
