/**
 *
 */
package io.github.roycetech.ruleengine;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import io.github.roycetech.ruleengine.converter.ElementConverter;

/**
 * Represents a rule token.
 *
 * @author royce
 */
public class Token {

	/**
	 * String or Array of tokens.
	 */
	private Object value;

	/**
	 * -1 for non-indexed, otherwise the index of the token.
	 */
	private final int subscript;

	/**
	 * Instantiate given a raw token string value.
	 *
	 * @param rawValue string to be parsed for the token body and it's subscript.
	 */
	public Token(final String rawValue) {
		this(extractBody(rawValue), extractSubscript(rawValue));
	}

	/**
	 * Instantiate based on the given token body and index.
	 *
	 * @param value     the body of the token.
	 * @param subscript the index of the token.
	 */
	public Token(final Object value, final int subscript) {
		this.value = value;
		this.subscript = subscript;
	}

	/**
	 * Checks if subscript is negative, meaning this token is non-indexed.
	 *
	 * @return true for negative subscript.
	 */
	public boolean isNegative()
	{
		return this.subscript < 0;
	}

	/**
	 * Checks if internal boolean matches this token's value.
	 *
	 * @param internal either '*true' or '*false'
	 * @return true if the internal passed matches this value.
	 */
	public boolean equalsInternal(final String internal)
	{
		return isNegative() && this.value.equals(internal);
	}

	/**
	 * Checks if this token is considered given the scenario list.
	 *
	 * @param scenario List of scenario tokens.
	 * @return true if this token is non-index (-1 subscript) and the scenario list
	 *         contained this token's value or, this token is subscripted and the
	 *         token value at the token index in the scenario matches.
	 */
	public boolean accepts(final List<Object> scenario)
	{
		return isNegative() ? scenario.contains(this.value)
				: scenario.get(this.subscript).equals(this.value);
	}

	/**
	 * Evaluates if this token's converted value satisfies the given scenario.
	 *
	 * @param scenario  target scenario to evaluate against.
	 * @param converter converter instance to apple to this token's value before
	 *                  qualifying this token against the scenario.
	 * @return true of this token satisfies the given scenario.
	 */
	public boolean accepts(final List<Object> scenario, final ElementConverter converter)
	{

		final Object convertedValue = converter.convert((String) this.value);
		return isNegative() ? scenario.contains(convertedValue)
				: scenario.get(this.subscript).equals(convertedValue);
	}

	/**
	 * Converts this token's value with the given converter instance.
	 *
	 * @param converter converter instance to convert this token's value.
	 */
	public void convert(final ElementConverter converter)
	{
		this.value = converter.convert((String) this.value);
	}

	/**
	 * Returns value of 'n' if rule token ends with '[n]'. where 'n' is the variable
	 * group index.
	 *
	 * @param string token to check for subscript.
	 */
	private static String extractBody(final String token)
	{
		final int bracketIndex = token.indexOf('[');
		if (bracketIndex > -1) {
			return token.substring(0, bracketIndex);
		}
		return token;
	}

	/**
	 * Returns value of 'n' if rule token ends with '[n]'. where 'n' is the variable
	 * group index.
	 *
	 * @param string token to check for subscript.
	 */
	private static int extractSubscript(final String token)
	{
		final String tokenString = token;

		final Pattern pattern = Pattern.compile(".*\\[[\\d*]\\]");
		final Matcher matcher = pattern.matcher(tokenString);
		if (matcher.find()) {
			final String indexStr = tokenString.substring(tokenString.indexOf('[') + 1,
					tokenString.indexOf(']'));
			return Integer.parseInt(indexStr);
		}
		return -1;
	}

	/**
	 * Gets the value of the token.
	 *
	 * @return the value of the token.
	 */
	public Object getValue()
	{
		return value;
	}

	@Override
	public String toString()
	{
		return String.valueOf(this.value) + '[' + this.subscript + ']';
	}
}
