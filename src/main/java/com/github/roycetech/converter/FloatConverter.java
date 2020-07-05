package com.github.roycetech.converter;

/**
 * @author Royce Remulla
 */
public class FloatConverter implements ElementConverter<Float> {

    /** {@inheritDoc} */
    @Override
    public Float convert(final String string) {
	return Float.parseFloat(string);
    }
}