/**
 *
 */
package io.github.roycetech.ruleengine.converter;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import io.github.roycetech.ruleengine.converter.FloatConverter;

/**
 * @author royce
 *
 */
public class FloatConverterTest {

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.converter.FloatConverter#convert(java.lang.String)}.
	 */
	@Test
	public final void testConvert_classOK()
	{
		final Object actual = new FloatConverter().convert("1");
		assertEquals(Float.class, actual.getClass());
	}
}
