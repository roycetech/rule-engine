package com.github.roycetech.ruleengine.converter;

/**
 * Converts a string into a java boolean type by parsing.
 */
@SuppressWarnings("java:S2055")
public class BooleanConverter implements ElementConverter {

	/**
	 * Default constructor for the BooleanConverter class. This constructor is
	 * provided by the compiler and requires no arguments.
	 */
	public BooleanConverter() {
	}

	/** {@inheritDoc} */
	@Override
	public Boolean convert(final String string)
	{
		return Boolean.parseBoolean(string);
	}
}