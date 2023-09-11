/**
 *
 */
package com.github.roycetech.converter;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * @author royce
 *
 */
public class IntConverterTest {

    /**
     * Test method for
     * {@link com.github.roycetech.converter.IntegerConverter#convert(java.lang.String)}.
     */
    @Test
    public final void testConvert_classOK()
    {
	final Object actual = new IntegerConverter().convert("1");
	assertEquals(Integer.class, actual.getClass());
    }

}
