package com.github.roycetech.converter;

/**
 * @author Royce Remulla
 * @param <T>
 */
public class BoolConverter implements ElementConverter {

    /** {@inheritDoc} */
    @Override
    public Boolean convert(final String string)
    {
	return Boolean.parseBoolean(string);
    }
}