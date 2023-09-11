package com.github.roycetech.converter;

/**
 * Converts a string into a java Integer type.
 */
public class IntegerConverter implements ElementConverter {

	/**
	 * Default constructor for the BooleanConverter class. This constructor is
	 * provided by the compiler and requires no arguments.
	 */
	public IntegerConverter() {
	}

	/** {@inheritDoc} */
	@Override
	public Object convert(final String string)
	{
		return Integer.parseInt(string);
	}
}