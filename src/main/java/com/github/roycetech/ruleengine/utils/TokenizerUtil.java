/**
 *
 */
package com.github.roycetech.ruleengine.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * The `TokenizerUtil` class provides utility methods for tokenizing strings
 * using specified delimiters.
 *
 * <p>
 * This class is designed to break down a given string into tokens using a set
 * of specified delimiters. It is commonly used in parsing and tokenization
 * tasks, such as those performed by the shunting-yard algorithm.
 */
public final class TokenizerUtil {

	/**
	 * Private constructor to prevent instantiation of this utility class.
	 */
	private TokenizerUtil() {
	}

	/**
	 * Tokenizes a given rule clause using the specified tokens as delimiters.
	 *
	 * @param clause The rule clause to be tokenized.
	 * @param tokens The delimiters used to break down the clause into tokens.
	 * @return An array of tokens representing the tokenized representation of the
	 *         clause.
	 */
	public static Object[] tokenize(final String clause, final String tokens)
	{
		final StringTokenizer stringTokenizer = new StringTokenizer(clause, tokens, true);

		final List<String> retval = new ArrayList<>();

		/* loop for handling each token - shunting-yard algorithm */
		while (stringTokenizer.hasMoreTokens()) {
			final String token = stringTokenizer.nextToken().trim();
			retval.add(token);
		}
		return retval.toArray(new Object[0]);
	}

}
