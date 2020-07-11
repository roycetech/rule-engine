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

    @Test
    public final void testConvert_classOK()
    {
	final Object actual = new IntConverter().convert("1");
	assertEquals(Integer.class, actual.getClass());
    }

}
