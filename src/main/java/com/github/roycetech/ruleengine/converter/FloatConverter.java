package com.github.roycetech.ruleengine.converter;

/**
 * Converts a string into a java float type.
 */
public class FloatConverter implements ElementConverter {

	/**
	 * Default constructor for the BooleanConverter class. This constructor is
	 * provided by the compiler and requires no arguments.
	 */
	public FloatConverter() {
	}

	/** {@inheritDoc} */
	@Override
	public Object convert(final String string)
	{
		return Float.parseFloat(string);
	}
}