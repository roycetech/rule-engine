package com.github.roycetech.converter;

/**
 * @author Royce Remulla
 */
public class IntConverter implements ElementConverter<Integer> {

    /** {@inheritDoc} */
    @Override
    public Integer convert(final String string) {
	return Integer.parseInt(string);
    }
}