package com.github.roycetech.converter;

/**
 * This is the default implementation.
 *
 * @author Royce Remulla
 */
public class StrConverter implements ElementConverter {

    /** {@inheritDoc} */
    @Override
    public String convert(final String string)
    {
	if ("null".equals(string)) {
	    return null;
	}

	return string;
    }
}