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
     * @param <T>    Scenario element type.
     * @param string element to convert.
     */
    T convert(String string);

}