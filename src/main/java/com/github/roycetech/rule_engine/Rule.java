/**
 *
 */
package com.github.roycetech.rule_engine;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Represents a logical rule object.
 *
 * @author royce
 */
@SuppressWarnings("PMD.ShortClassName")
public class Rule {

    /**
     * Contains the outcome to rule-clause hash.
     */
    private final Map<String, String> outcomeClauseHash;

    /**
     * Instantiate a rule with the given clause. <br>
     * <b>Parameter Example:</b><br>
     * Visible: Proposed|Approved<br>
     * <br>
     *
     * @param rules the outcome to clause mapping.
     */
    public Rule(final Map<String, String> rules) {

	if (rules.isEmpty()) {
	    throw new IllegalArgumentException("Must not have empty rules");
	}

	this.outcomeClauseHash = new HashMap<>();

	rules.forEach((outcome, clause) -> this.outcomeClauseHash.put(outcome,
		Rule.sanitize(clause)));
    }

    /**
     *
     * @param clause dynamic, can be a String or an Array, blech.
     * @return
     */
    static String sanitize(final String clause)
    {
	if (clause.getClass().isArray()) {
	    return clause;
	}

	String cleaner = Rule.removeSpaces(clause, '(');
	cleaner = Rule.removeSpaces(cleaner, ')');
	cleaner = Rule.removeSpaces(cleaner, '&');
	cleaner = Rule.removeSpaces(cleaner, '|');

	return Rule.removeSpaces(cleaner, '!').strip();
    }

    int getSize()
    {
	return getOutcomes().size();
    }

    /**
     * Removes the leading and trailing spaces of rule tokens.
     *
     * @param string    rule clause.
     * @param separator rule clause token.
     */
    static String removeSpaces(final String token, final char separator)
    {
	return token.replaceAll("\\s*" + separator + "\\s*",
		String.valueOf(separator));
    }

    /**
     * @return the outcomes list.
     */
    Set<String> getOutcomes()
    {
	return this.outcomeClauseHash.keySet();
    }

    /**
     * @param outcome the outcome with which we want to get the clause of.
     *
     * @return the rule clause.
     */
    public String getClause(final String outcome)
    {
	return this.outcomeClauseHash.get(outcome);
    }
}
