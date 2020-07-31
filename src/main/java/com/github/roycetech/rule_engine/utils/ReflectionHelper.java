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

    /**
     * Target method to invoke.
     */
    private final Method method;

    /**
     * @param klass          the class that has the method definition.
     * @param methodName     the name of the method.
     * @param parameterTypes the parameter array.
     *
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
     * Forces access to otherwise inaccessible methods.
     */
    public void forceAccess()
    {
	this.method.setAccessible(true);
    }
}
