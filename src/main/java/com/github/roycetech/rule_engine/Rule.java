/**
 *
 */
package com.github.roycetech.rule_engine;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.github.roycetech.rule_engine.utils.StringUtil;

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
	private final Map<String, Object> outcomeClauseHash;

	/**
	 * Instantiate a rule with the given clause. <br>
	 * <b>Parameter Example:</b><br>
	 * Visible: Proposed|Approved<br>
	 * <br>
	 *
	 * @param rules the outcome to clause mapping.
	 */
	public Rule(final Map<String, Object> rules) {
		if (rules == null || rules.isEmpty()) {
			throw new IllegalArgumentException("Must not have empty rules");
		}

		this.outcomeClauseHash = new LinkedHashMap<>();

		rules.forEach((outcome, clause) -> this.outcomeClauseHash.put(outcome, Rule.sanitize(clause)));
	}

	/**
	 * Removes unwanted spaces between operators.
	 *
	 * @param clause dynamic, can be a String or an Array, blech.
	 * @return the sanitized clause.
	 */
	static Object sanitize(final Object clause)
	{
		if (clause.getClass().isArray()) {
			return clause;
		}

		final String clauseString = (String) clause;

		String cleaner = Rule.removeSpaces(clauseString, "\\(");
		cleaner = Rule.removeSpaces(cleaner, "\\)");
		cleaner = Rule.removeSpaces(cleaner, "&");
		cleaner = Rule.removeSpaces(cleaner, "\\|");

		return Rule.removeSpaces(cleaner, "!").trim();
	}

	int getSize()
	{
		return getOutcomes().size();
	}

	/**
	 * Removes the leading and trailing spaces of rule tokens.
	 *
	 * @param string    rule clause.
	 * @param separator rule engine operator. Regex like operators must be escaped.
	 */
	static String removeSpaces(final String token, final String separator)
	{
		return token.replaceAll("\\s*" + separator + "\\s*", String.valueOf(separator));
	}

	/**
	 * Get rule result give a fixed list of scenario tokens. Used for fixed list.
	 * 
	 * @param scenario of interest.
	 * @return the actionToRuleClauses
	 */
	@SuppressWarnings("PMD.OnlyOneReturn" /* Two only. */)
	public String getRuleOutcome(final List<String> scenario)
	{
		assert scenario != null;

		final String scenStr = scenario.toString();
		final String andedScen = scenStr.substring(1, scenStr.length() - 1).replaceAll(", ", "&");

		for (final String key : outcomeClauseHash.keySet()) {
			final String clause = (String) outcomeClauseHash.get(key);

			final List<String> orListClause = Arrays.asList(StringUtil.trimArray(clause.split("\\|")));
			if (orListClause.contains(andedScen)) {
				return key;
			}
		}
		return null;
	}

	/**
	 * @return the outcomes list.
	 */
	public Set<String> getOutcomes()
	{
		return this.outcomeClauseHash.keySet();
	}

	/**
	 * @param outcome the outcome with which we want to get the clause of.
	 *
	 * @return the rule clause.
	 */
	public Object getClause(final String outcome)
	{
		return this.outcomeClauseHash.get(outcome);
	}
}
