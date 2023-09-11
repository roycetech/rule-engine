/**
 *   Copyright 2013 Royce Remulla
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License. 
 */
package com.github.roycetech.rule_engine.utils;

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
	 * Trims the String content on an array of String.
	 * 
	 * @param array String array to trim contents.
	 */
	public static String[] trimArray(final String... array)
	{
		if (array == null)
			return null;

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
	 * True if string has non-null and not empty.
	 * 
	 * @param string string to check.
	 */
	public static boolean hasValue(final String string)
	{
		return string != null && !"".equals(string.trim());
	}

}
