package com.github.roycetech.converter;

/**
 * @author Royce Remulla
 */
public class DblConverter implements ElementConverter<Double> {

    /** {@inheritDoc} */
    @Override
    public Double convert(final String string) {
	return Double.parseDouble(string);
    }
}