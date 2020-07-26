package com.github.roycetech.converter;

/**
 * Scenario element converter. Does not currently support custom converter,
 * since it need to apply on the rule evaluation as well.
 *
 *
 * @author Royce Remulla
 */
public interface ElementConverter {

    /**
     * Convert string to correct data type.
     *
     * @param string element to convert.
     *
     * @return the string converted to it's actual type.
     */
    Object convert(String string);

}