package io.github.roycetech.ruleengine.utils;

/**
 * Some util for String manipulation.
 *
 * @author Royce Remulla
 */
@SuppressWarnings("PMD.ClassWithOnlyPrivateConstructorsShouldBeFinal" /* For easy testability. */)
public class StringUtil {

	/** Utility class. */
	private StringUtil() {
	}

	/**
	 * Trims the String content in an array of String.
	 *
	 * @param array String array to trim contents.
	 * @return A new array containing trimmed versions of the input strings.
	 */
	public static String[] trimArray(final String... array)
	{
		if (array == null)
			return null; // NOPMD: null in null out.

		final String[] retval = new String[array.length];
		System.arraycopy(array, 0, retval, 0, array.length);
		for (int i = 0; i < retval.length; i++) {
			if (array[i] == null) {
				retval[i] = array[i];
			} else {
				retval[i] = array[i].trim();
			}
		}

		return retval;
	}

	/**
	 * Checks if a string has a non-null and non-empty value.
	 *
	 * @param string The string to check.
	 * @return {@code true} if the string is not null and not empty, {@code false}
	 *         otherwise.
	 */
	public static boolean hasValue(final String string)
	{
		return string != null && !"".equals(string.trim());
	}

}
