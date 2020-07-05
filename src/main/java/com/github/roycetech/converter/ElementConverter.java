package com.github.roycetech.converter;

/**
 * Scenario element converter. Does not currently support custom converter,
 * since it need to apply on the rule evaluation as well.
 *
 * @param <T> Scenario element type.
 *
 * @author Royce Remulla
 */
public interface ElementConverter<T> {

    /**
     * Convert string to correct data type.
     *
     * @param string element to convert.
     * @return the string converted to it's actual type.
     */
    T convert(String string);

}