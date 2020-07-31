/**
 *
 */
package com.github.roycetech.rule_engine.utils;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import com.github.roycetech.rule_engine.RuleEngineException;

/**
 * Helper class to hide the dirty details of handling checked exceptions in a
 * reflection call.
 *
 * @author royce
 *
 */
public class ReflectionHelper {

    private final Method method;

    /**
     * @param klass
     * @param methodName
     * @param parameterTypes
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
     * Invokes the method with the arguments and returns the result or throws
     * the exception encountered during invocation.
     *
     * @param <T>
     * @param target where the method is to be invoked, null for static
     *               invocation.
     * @param args   arguments to the invocation.
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
     * Forces access to otherwise inaccessible methods.
     */
    public void forceAccess()
    {
	this.method.setAccessible(true);
    }
}
