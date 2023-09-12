package com.github.roycetech.ruleengine.converter;

/**
 * Converts a string into a java double type.
 */
public class DoubleConverter implements ElementConverter {

	/**
	 * Default constructor for the BooleanConverter class. This constructor is
	 * provided by the compiler and requires no arguments.
	 */
	public DoubleConverter() {
	}

	/** {@inheritDoc} */
	@Override
	public Object convert(final String string)
	{
		return Double.parseDouble(string);
	}
}