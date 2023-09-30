/**
 *
 */
package io.github.roycetech.ruleengine.converter;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import io.github.roycetech.ruleengine.converter.DoubleConverter;

/**
 * @author royce
 *
 */
public class DblConverterTest {

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.converter.DoubleConverter#convert(java.lang.String)}.
	 */
	@Test
	public final void testConvert_classOK()
	{
		final Object actual = new DoubleConverter().convert("1");
		assertEquals(Double.class, actual.getClass());
	}

}
