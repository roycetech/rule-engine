package com.github.roycetech.converter;

/**
 * This is the default implementation.
 *
 * @author Royce Remulla
 */
public class StrConverter implements ElementConverter<String> {

    /** {@inheritDoc} */
    @Override
    public String convert(final String string)
    {
	if (string == null) {
	    return null;
	}

	return string;
    }
}