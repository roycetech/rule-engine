/**
 * The `RuleProcessor` class is responsible for evaluating rules based on a given scenario
 * and a set of outcomes. It utilizes a `Rule` object and a map of token converters
 * to perform the evaluation.
 *
 * <p>This class provides functionality to evaluate a set of predefined rules against a given
 * scenario. It allows you to determine the outcomes associated with the provided scenario.
 */
package io.github.roycetech.ruleengine;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import io.github.roycetech.ruleengine.converter.ElementConverter;

/**
 * The RuleProcessor class handles the evaluation of rules.
 */
public class RuleProcessor {

	/** Rule model containing the outcomes and the clause. */
	private final Rule rule;

	/**
	 * Token-dependent converters.
	 */
	private final Map<String, ElementConverter> tokenConverters;

	/**
	 * Constructs a `RuleProcessor` instance with a given `Rule` object and a map of
	 * token converters.
	 *
	 * @param rule            The `Rule` object representing the predefined rules to
	 *                        be evaluated.
	 * @param tokenConverters A map of token converters used for converting tokens
	 *                        in the rule clauses.
	 */
	public RuleProcessor(final Rule rule, final Map<String, ElementConverter> tokenConverters) {
		this.rule = rule;
		this.tokenConverters = tokenConverters;
	}

	/**
	 * Evaluates the predefined rules against the provided scenario and returns an
	 * array of Boolean values representing the outcomes for each rule.
	 *
	 * @param scenario The current scenario to evaluate against the predefined
	 *                 rules.
	 * @return An array of Boolean values indicating the outcomes for each rule.
	 */
	public Boolean[] evaluate(final List<Object> scenario)
	{
		final List<Boolean> retval = new ArrayList<>();
		for (final String outcome : this.rule.getOutcomes()) {
			retval.add(checkScenarioAgainstOutcome(scenario, outcome));
		}
		return retval.toArray(new Boolean[0]);
	}

	/**
	 * Checks the provided scenario against a specific target outcome and returns a
	 * Boolean value indicating whether the scenario matches the outcome's rule
	 * clause.
	 *
	 * @param scenario      The current scenario to evaluate.
	 * @param targetOutcome The target outcome for which to perform the evaluation.
	 * @return `true` if the scenario matches the rule clause of the target outcome;
	 *         otherwise, `false`.
	 */
	private boolean checkScenarioAgainstOutcome(final List<Object> scenario,
			final String targetOutcome)
	{
		final Object clause = rule.getClause(targetOutcome);
		final RuleEvaluator ruleEvaluator = new RuleEvaluator(this.tokenConverters);
		ruleEvaluator.parse(clause);
		return ruleEvaluator.evaluate(scenario);
	}

}
