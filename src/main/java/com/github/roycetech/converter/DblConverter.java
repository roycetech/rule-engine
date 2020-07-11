package com.github.roycetech.converter;

/**
 * @author Royce Remulla
 */
public class DblConverter implements ElementConverter {

    /** {@inheritDoc} */
    @Override
    public Object convert(final String string)
    {
	return Double.parseDouble(string);
    }
}