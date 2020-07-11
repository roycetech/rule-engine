package com.github.roycetech.converter;

/**
 * @author Royce Remulla
 */
public class IntConverter implements ElementConverter {

    /** {@inheritDoc} */
    @Override
    public Object convert(final String string)
    {
	return Integer.parseInt(string);
    }
}