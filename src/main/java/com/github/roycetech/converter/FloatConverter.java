package com.github.roycetech.converter;

/**
 * @author Royce Remulla
 */
public class FloatConverter implements ElementConverter {

    /** {@inheritDoc} */
    @Override
    public Object convert(final String string)
    {
	return Float.parseFloat(string);
    }
}