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
public final class ClauseTokenizer {

    private ClauseTokenizer() {
    }

    /**
     * @clause - rule clause to be tokenized
     * @tokens - the tokens to be used to break down the clause.
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
