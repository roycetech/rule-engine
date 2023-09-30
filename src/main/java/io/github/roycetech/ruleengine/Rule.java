/**
 *
 */
package io.github.roycetech.ruleengine;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import io.github.roycetech.ruleengine.utils.StringUtil;

/**
 * The `Rule` class represents a logical rule object that associates outcomes
 * with their corresponding clauses. It allows you to define rules and evaluate
 * scenarios against those rules to determine the applicable outcomes.
 *
 * <p>
 * Each rule consists of multiple outcomes, and each outcome is associated with
 * a clause.
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

		rules.forEach(
				(outcome, clause) -> this.outcomeClauseHash.put(outcome, Rule.sanitize(clause)));
	}

	/**
	 * Static utility method to remove unwanted spaces between operators in a rule
	 * clause.
	 *
	 * @param clause The rule clause, which can be a String or an Array.
	 * @return The sanitized clause with spaces removed between operators.
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

	/**
	 * Convenience method to determine the number of possible outcomes.
	 *
	 * @return the current number of possible outcomes.
	 */
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

			final List<String> orListClause = Arrays
					.asList(StringUtil.trimArray(clause.split("\\|")));
			if (orListClause.contains(andedScen)) {
				return key;
			}
		}
		return null;
	}

	/**
	 * Gets the set of available outcomes defined in the rule.
	 *
	 * @return A set containing all the available outcomes.
	 */
	public Set<String> getOutcomes()
	{
		return this.outcomeClauseHash.keySet();
	}

	/**
	 * Gets the rule clause associated with a specific outcome.
	 *
	 * @param outcome The outcome for which to retrieve the rule clause.
	 * @return The rule clause corresponding to the specified outcome.
	 */
	public Object getClause(final String outcome)
	{
		return this.outcomeClauseHash.get(outcome);
	}
}
