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

    public PrivateMethodInvoker(final Class<?> klass, final String methodName,
	    final Class<?>... argClasses) {
	try {
	    this.method = klass.getDeclaredMethod(methodName, argClasses);
	} catch (NoSuchMethodException | SecurityException e) {
	    // TODO Auto-generated catch block
	    e.printStackTrace();
	}
    }

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
