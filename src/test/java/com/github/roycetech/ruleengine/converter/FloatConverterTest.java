/**
 *
 */
package com.github.roycetech.ruleengine.converter;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * @author royce
 *
 */
public class FloatConverterTest {

	/**
	 * Test method for
	 * {@link com.github.roycetech.ruleengine.converter.FloatConverter#convert(java.lang.String)}.
	 */
	@Test
	public final void testConvert_classOK()
	{
		final Object actual = new FloatConverter().convert("1");
		assertEquals(Float.class, actual.getClass());
	}
}
