/**
 *
 */
package com.github.roycetech.rule_engine.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * @author royce
 *
 */
public final class TokenizerUtil {

    private TokenizerUtil() {
    }

    /**
     * @param clause - rule clause to be tokenized
     * @param tokens - the tokens to be used to break down the clause.
     * @return the tokenized representation of the clause.
     */
    public static Object[] tokenize(final String clause, final String tokens)
    {
	final StringTokenizer stringTokenizer = new StringTokenizer(clause,
		tokens, true);

	final List<String> retval = new ArrayList<>();

	/* loop for handling each token - shunting-yard algorithm */
	while (stringTokenizer.hasMoreTokens()) {
	    final String token = stringTokenizer.nextToken().trim();
	    retval.add(token);
	}
	return retval.toArray(new Object[0]);
    }

}
