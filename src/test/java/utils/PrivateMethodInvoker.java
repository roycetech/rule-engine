/**
 *
 */
package utils;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Helper method for testing private methods. This class hides the ugly details
 * of the checked exception handling.
 *
 * @author royce
 *
 */
public class PrivateMethodInvoker {

	/**
	 *
	 */
	private Method method;

	/**
	 * @param klass      target class that defines the private method.
	 * @param methodName the private method to be invoked.
	 * @param argClasses parameter types of the private method.
	 */
	public PrivateMethodInvoker(final Class<?> klass, final String methodName,
			final Class<?>... argClasses) {
		try {
			this.method = klass.getDeclaredMethod(methodName, argClasses);
		} catch (NoSuchMethodException | SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Sets the private method to accessible and invokes it on the target.
	 *
	 * @param <T>    the type to which to cast the result.
	 * @param target null for static or the instance on which to call the method.
	 * @param params parameters to the private method.
	 * @return the result of the private method invocation.
	 */
	@SuppressWarnings("unchecked")
	public <T> T invoke(final Object target, final Object... params)
	{
		try {
			method.setAccessible(true);
			return (T) method.invoke(target, params);
		} catch (final InvocationTargetException e) {
			throw (RuntimeException) e.getTargetException();
		} catch (IllegalAccessException | IllegalArgumentException e) {
			e.printStackTrace();
			return null;
		}
	}
}
