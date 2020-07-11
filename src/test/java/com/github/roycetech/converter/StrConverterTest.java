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
public class StrConverterTest {

    /**
     * Test method for
     * {@link com.github.roycetech.converter.StrConverter#convert(java.lang.String)}.
     */
    @Test
    public final void testConvert_classOK()
    {
	final Object actual = new StrConverter().convert("1");
	assertEquals(String.class, actual.getClass());
    }

    /**
     * Test method for
     * {@link com.github.roycetech.converter.StrConverter#convert(java.lang.String)}.
     */
    @Test
    public final void testConvert_null()
    {
	final Object actual = new StrConverter().convert("null");
	assertEquals(null, actual);
    }

}
