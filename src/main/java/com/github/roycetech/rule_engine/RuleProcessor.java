/**
 *
 */
package com.github.roycetech.rule_engine;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.github.roycetech.converter.ElementConverter;

/**
 * @author royce
 *
 */
public class RuleProcessor {

    /** Rule model containing the outcomes and the clause. */
    private final Rule rule;

    /**
     * Token dependent converters.
     */
    private final Map<String, ElementConverter<?>> tokenConverters;

    RuleProcessor(Rule rule, Map<String, ElementConverter<?>> tokenConverters) {
	this.rule = rule;
	this.tokenConverters = tokenConverters;
    }

    /**
     * @param scenario    current scenario.
     * @param caseFixture current test case fixture.
     */
    Boolean[] evaluate(List<Object> scenario)
    {
	final List<Boolean> retval = new ArrayList<>();

	for (final String outcome : this.rule.getOutcomes()) {
	    retval.add(checkScenarioAgainstOutcome(scenario, outcome));
	}

	return retval.toArray(new Boolean[retval.size()]);
    }

    private boolean checkScenarioAgainstOutcome(List<Object> scenario,
	    final String targetOutcome)
    {

	final String clause = rule.getClause(targetOutcome);
	final RuleEvaluator ruleEvaluator = new RuleEvaluator(
		this.tokenConverters);

	ruleEvaluator.parse(clause);

	return ruleEvaluator.evaluate(scenario);
    }

}
