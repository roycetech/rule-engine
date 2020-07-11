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
public class DblConverterTest {

    /**
     * Test method for
     * {@link com.github.roycetech.converter.DblConverter#convert(java.lang.String)}.
     */
    @Test
    public final void testConvert_classOK()
    {
	final Object actual = new DblConverter().convert("1");
	assertEquals(Double.class, actual.getClass());
    }

}
