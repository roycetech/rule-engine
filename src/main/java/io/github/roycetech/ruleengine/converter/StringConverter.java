package io.github.roycetech.ruleengine.converter;

/**
 * This is the default implementation which converts a string literal "null" to
 * a null or returns the string as is.
 */
public class StringConverter implements ElementConverter {

	/**
	 * Default constructor for the BooleanConverter class. This constructor is
	 * provided by the compiler and requires no arguments.
	 */
	public StringConverter() {
	}

	/** {@inheritDoc} */
	@Override
	public String convert(final String string)
	{
		return "null".equals(string) ? null : string;
	}
}